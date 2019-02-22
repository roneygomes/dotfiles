alias dynamodb="java \
    -Djava.library.path=$TOOLS_DIR/dynamodb/DynamoDBLocal_lib \
    -jar $TOOLS_DIR/dynamodb/DynamoDBLocal.jar \
    -sharedDb"
