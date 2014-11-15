
# Declare exchange
rabbitmqadmin declare exchange name=my-new-exchange type=fanout

# Declare queue
rabbitmqadmin declare queue name=my-new-queue durable=false

# Publish message
rabbitmqadmin publish exchange=amq.default routing_key=test payload="hello, world"

# Get message
rabbitmqadmin get queue=test requeue=false

# Close all connections
rabbitmqadmin -f tsv -q list connections name | while read conn ; do rabbitmqadmin -q close connection name=${conn} ; done
