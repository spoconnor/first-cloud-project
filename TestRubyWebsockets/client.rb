# Copyright: Hiroshi Ichikawa <http://gimite.net/en/>
# Lincense: New BSD Lincense

$LOAD_PATH << File.dirname(__FILE__) + "/lib"
require "web_socket"

#if ARGV.size != 1
#  $stderr.puts("Usage: ruby samples/stdio_client.rb ws://HOST:PORT/")
#  exit(1)
#end

#begin

#client = WebSocket.new(ARGV[0])
client = WebSocket.new("ws://localhost:8081")
puts("Connected")
Thread.new() do
  while data = client.receive()
    printf("Received: %p\n", data)
  end
  printf("Closing reader")
  exit()
end

$stdin.each_line() do |line|
  data = line.chomp()
  client.send(data)
  printf("Sent: %p\n", data)
end
print("Client closing")
client.close()

#rescue Exception => e
# puts "Exception #{e}"
#end
