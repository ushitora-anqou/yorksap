#!/usr/bin/bash -xeu

set -o pipefail

print_error(){
  echo
  echo -e "\e[1;31m[ERROR]\e[0m $@" >&2
  echo
}

failwith(){
  print_error "$@"
  exit 1
}

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

TEST_NAME=$(date '+e2e-test-%Y%m%d%H%M%S')
SQLITE3_DB_FILE_PATH="$PWD/$TEST_NAME.sqlite3"
SERVER_URI="http://localhost:8080"

echo "TEST_NAME:            $TEST_NAME"
echo "SQLITE3_DB_FILE_PATH: $SQLITE3_DB_FILE_PATH"
echo "SERVER_URI:           $SERVER_URI"

# Launch the server with a fresh sqlite3 db file
OCAMLRUNPARAM=b DSN="sqlite3://$SQLITE3_DB_FILE_PATH" RANDOM_INIT=0 opam exec -- dune exec bin/main.exe server &
sleep 5 # Wait the server to be ready

get_user_name(){
  TURN=$1
  echo "ゆ〜ざ〜$TURN"
}
create_room(){
  ROOM_NAME=$(uuidgen)
  USER_NAME=$(get_user_name 1)
  curl -d "{\"roomName\":\"てすとる〜む $ROOM_NAME\",\"userName\":\"$USER_NAME\",\"userPassword\":\"ぱすわ〜ど１\"}" $SERVER_URI/api/v1/room
}
register_user(){
  TURN=$1
  ROOM_ID=$2
  USER_NAME=$(get_user_name $TURN)
  USER_PASSWORD=$(uuidgen)
  curl -d "{\"userName\":\"$USER_NAME\",\"userPassword\":\"$USER_PASSWORD\"}" "http://localhost:8080/api/v1/room/$ROOM_ID/register" | jq -r '.accessToken'
}
single_move(){
  ROOM_ID=$1
  ACCESS_TOKEN=$2
  BODY=$(\
    curl -H "Authorization: Bearer $ACCESS_TOKEN" "http://localhost:8080/api/v1/game/$ROOM_ID" | \
    jq -c '[.next[] | select(.[0] != "DOUBLE")][0] | { ticket: .[0], destination: .[1] }'\
  )
  curl -H "Authorization: Bearer $ACCESS_TOKEN" -d "$BODY" "http://localhost:8080/api/v1/game/$ROOM_ID/move"
}
double_move(){
  ROOM_ID=$1
  ACCESS_TOKEN=$2
  BODY=$(\
    curl -H "Authorization: Bearer $ACCESS_TOKEN" "http://localhost:8080/api/v1/game/$ROOM_ID" | \
    jq -c '[.next[] | select(.[0] == "DOUBLE")][0] | { ticket1: .[1][0], destination1: .[1][1], ticket2: .[2][0], destination2: .[2][1] }'\
  )
  curl -H "Authorization: Bearer $ACCESS_TOKEN" -d "$BODY" "http://localhost:8080/api/v1/game/$ROOM_ID/double-move"
}
get_game(){
  ROOM_ID=$1
  curl "http://localhost:8080/api/v1/game/$ROOM_ID"
}

# Test basic functionalities
test_case_1(){
  # Create a room
  JSON=$(create_room)
  ROOM_ID=$(echo $JSON | jq -r '.roomId')
  ACCESS_TOKEN1=$(echo $JSON | jq -r '.accessToken')

  # Register users
  ACCESS_TOKEN2=$(register_user 2 $ROOM_ID)
  ACCESS_TOKEN3=$(register_user 3 $ROOM_ID)
  ACCESS_TOKEN4=$(register_user 4 $ROOM_ID)
  ACCESS_TOKEN5=$(register_user 5 $ROOM_ID)
  ACCESS_TOKEN6=$(register_user 6 $ROOM_ID)

  # Check game is valid
  JSON=$(get_game $ROOM_ID)
  [ $(echo "$JSON" | jq -r '.phase') = "1" ] || failwith "invalid phase"
  [ $(echo "$JSON" | jq -r '.turn') = "ゆ〜ざ〜1" ] || failwith "invalid turn"
  [ $(echo "$JSON" | jq -r '.gameOver') = "false" ] || failwith "invalid gameOver"
  [ $(echo "$JSON" | jq -r '.gameStatus') = "0" ] || failwith "invalid gameStatus"
  [ $(echo "$JSON" | jq -r '.nowPosition[0].position') = "null" ] || failwith "invalid position"
  [ $(echo "$JSON" | jq -r '.nowPosition[1].position') != "null" ] || failwith "invalid position"
  [ $(echo "$JSON" | jq -r '.history[0].phase') = "0" ] || failwith "invalid history phase"
  [ $(echo "$JSON" | jq -r '.history[0].player[0].position') = "null" ] || failwith "invalid history player position"
  [ $(echo "$JSON" | jq -r '.ticket[0].TAXI') = "4" ] || failwith "invalid taxi ticket"
  [ $(echo "$JSON" | jq -r '.ticket[0].BUS') = "3" ] || failwith "invalid bus ticket"
  [ $(echo "$JSON" | jq -r '.ticket[0].UNDERGROUND') = "3" ] || failwith "invalid ug ticket"
  [ $(echo "$JSON" | jq -r '.ticket[0].SECRET') = "5" ] || failwith "invalid secret ticket"
  [ $(echo "$JSON" | jq -r '.ticket[0].DOUBLE') = "2" ] || failwith "invalid double ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].TAXI') = "10" ] || failwith "invalid taxi ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].BUS') = "8" ] || failwith "invalid bus ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].UNDERGROUND') = "4" ] || failwith "invalid ug ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].SECRET') = "0" ] || failwith "invalid secret ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].DOUBLE') = "0" ] || failwith "invalid double ticket"

  # single move
  single_move $ROOM_ID $ACCESS_TOKEN1

  # Check game is valid
  JSON=$(get_game $ROOM_ID)
  [ $(echo "$JSON" | jq -r '.phase') = "1" ] || failwith "invalid phase"
  [ $(echo "$JSON" | jq -r '.turn') = "ゆ〜ざ〜2" ] || failwith "invalid turn"
  [ $(echo "$JSON" | jq -r '.gameOver') = "false" ] || failwith "invalid gameOver"
  [ $(echo "$JSON" | jq -r '.gameStatus') = "0" ] || failwith "invalid gameStatus"
  [ $(echo "$JSON" | jq -r '.nowPosition[0].position') = "null" ] || failwith "invalid position"
  [ $(echo "$JSON" | jq -r '.nowPosition[1].position') != "null" ] || failwith "invalid position"
  [ $(echo "$JSON" | jq -r '.history[1].phase') = "1" ] || failwith "invalid history phase"
  [ $(echo "$JSON" | jq -r '.history[1].player[0].position') = "null" ] || failwith "invalid history player position"
  [ $(echo "$JSON" | jq -r '.ticket[0].TAXI') = "3" ] || failwith "invalid taxi ticket"
  [ $(echo "$JSON" | jq -r '.ticket[0].BUS') = "3" ] || failwith "invalid bus ticket"
  [ $(echo "$JSON" | jq -r '.ticket[0].UNDERGROUND') = "3" ] || failwith "invalid ug ticket"
  [ $(echo "$JSON" | jq -r '.ticket[0].SECRET') = "5" ] || failwith "invalid secret ticket"
  [ $(echo "$JSON" | jq -r '.ticket[0].DOUBLE') = "2" ] || failwith "invalid double ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].TAXI') = "10" ] || failwith "invalid taxi ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].BUS') = "8" ] || failwith "invalid bus ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].UNDERGROUND') = "4" ] || failwith "invalid ug ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].SECRET') = "0" ] || failwith "invalid secret ticket"
  [ $(echo "$JSON" | jq -r '.ticket[1].DOUBLE') = "0" ] || failwith "invalid double ticket"

  # single moves to next turn
  single_move $ROOM_ID $ACCESS_TOKEN2
  single_move $ROOM_ID $ACCESS_TOKEN3
  single_move $ROOM_ID $ACCESS_TOKEN4
  single_move $ROOM_ID $ACCESS_TOKEN5
  single_move $ROOM_ID $ACCESS_TOKEN6

  # Check game is valid
  JSON=$(get_game $ROOM_ID)
  [ $(echo "$JSON" | jq -r '.phase') = "2" ] || failwith "invalid phase"
  [ $(echo "$JSON" | jq -r '.turn') = "ゆ〜ざ〜1" ] || failwith "invalid turn"
  [ $(echo "$JSON" | jq -r '.gameOver') = "false" ] || failwith "invalid gameOver"
  [ $(echo "$JSON" | jq -r '.gameStatus') = "0" ] || failwith "invalid gameStatus"
  [ $(echo "$JSON" | jq -r '.nowPosition[0].position') = "null" ] || failwith "invalid position"
  [ $(echo "$JSON" | jq -r '.nowPosition[1].position') != "null" ] || failwith "invalid position"

  # single moves (2:00 a.m.)
  single_move $ROOM_ID $ACCESS_TOKEN1
  single_move $ROOM_ID $ACCESS_TOKEN2
  single_move $ROOM_ID $ACCESS_TOKEN3
  single_move $ROOM_ID $ACCESS_TOKEN4
  single_move $ROOM_ID $ACCESS_TOKEN5
  single_move $ROOM_ID $ACCESS_TOKEN6

  # Check game is valid
  JSON=$(get_game $ROOM_ID)
  [ $(echo "$JSON" | jq -r '.phase') = "3" ] || failwith "invalid phase"
  [ $(echo "$JSON" | jq -r '.turn') = "ゆ〜ざ〜1" ] || failwith "invalid turn"
  [ $(echo "$JSON" | jq -r '.nowPosition[0].position') = "null" ] || failwith "invalid position"
  [ $(echo "$JSON" | jq -r '.nowPosition[1].position') != "null" ] || failwith "invalid position"

  # single moves (3:00 a.m.)
  single_move $ROOM_ID $ACCESS_TOKEN1

  # Check game is valid
  JSON=$(get_game $ROOM_ID)
  [ $(echo "$JSON" | jq -r '.phase') = "3" ] || failwith "invalid phase"
  [ $(echo "$JSON" | jq -r '.turn') = "ゆ〜ざ〜2" ] || failwith "invalid turn"
  [ $(echo "$JSON" | jq -r '.nowPosition[0].position') = "95" ] || failwith "invalid position"

  # single moves (3:00 a.m.)
  single_move $ROOM_ID $ACCESS_TOKEN2
  single_move $ROOM_ID $ACCESS_TOKEN3
  single_move $ROOM_ID $ACCESS_TOKEN4
  single_move $ROOM_ID $ACCESS_TOKEN5
  single_move $ROOM_ID $ACCESS_TOKEN6

  # double move (4:00 a.m.)
  double_move $ROOM_ID $ACCESS_TOKEN1

  # Check game is valid
  JSON=$(get_game $ROOM_ID)
  [ $(echo "$JSON" | jq -r '.phase') = "5" ] || failwith "invalid phase"
  [ $(echo "$JSON" | jq -r '.turn') = "ゆ〜ざ〜2" ] || failwith "invalid turn"
}

# Test to check if turns are correctly skipped
test_case_2(){
  # Create a room
  JSON=$(create_room)
  ROOM_ID=$(echo $JSON | jq -r '.roomId')
  ACCESS_TOKEN1=$(echo $JSON | jq -r '.accessToken')

  # Register users
  ACCESS_TOKEN2=$(register_user 2 $ROOM_ID)
  ACCESS_TOKEN3=$(register_user 3 $ROOM_ID)
  ACCESS_TOKEN4=$(register_user 4 $ROOM_ID)
  ACCESS_TOKEN5=$(register_user 5 $ROOM_ID)
  ACCESS_TOKEN6=$(register_user 6 $ROOM_ID)

  for i in $(seq 1 22); do
    single_move $ROOM_ID $ACCESS_TOKEN1
    single_move $ROOM_ID $ACCESS_TOKEN2 || true
    single_move $ROOM_ID $ACCESS_TOKEN3 || true
    single_move $ROOM_ID $ACCESS_TOKEN4 || true
    single_move $ROOM_ID $ACCESS_TOKEN5 || true
    single_move $ROOM_ID $ACCESS_TOKEN6 || true
  done
  single_move $ROOM_ID $ACCESS_TOKEN1
  JSON=$(get_game $ROOM_ID)
  [ $(echo $JSON | jq '.gameOver') = "false" ] || failwith "game should not be over"
  single_move $ROOM_ID $ACCESS_TOKEN1
  JSON=$(get_game $ROOM_ID)
  [ $(echo $JSON | jq '.gameOver') = "true" ] || failwith "game should be over"
}

case "$1" in
  test_case_1 )
    test_case_1
    ;;
  test_case_2 )
    test_case_2
    ;;
esac

kill %1
trap - SIGINT SIGTERM EXIT
rm $SQLITE3_DB_FILE_PATH
