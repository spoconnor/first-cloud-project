# Copyright: Hiroshi Ichikawa <http://gimite.net/en/>
# Lincense: New BSD Lincense

$LOAD_PATH << File.dirname(__FILE__) + "/lib"
require "web_socket"

$LOAD_PATH << File.dirname(__FILE__) + "/../ProtoBufs"
require "CommsMessages.pb.rb"

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

while (1) do
  puts("")
  puts("[1] register")
  puts("[2] say")
  puts("[3] move")
  puts("[4] exit")
  selection = gets.chomp
  case selection
  when "1"
    puts "Register"
    msg = CommsMessages::RegisterClientRequest.new
    printf("Name:")
    msg.name = gets.chomp
    bin = msg.to_s
    client.send("register|" + bin)
  when "2"
    puts "Say"
    msg = CommsMessages::Message.new
    printf("Message:")
    msg.from = "me"
    msg.target = "you"
    msg.message = gets.chomp
    bin = msg.to_s
    client.send("say|" + bin)
  when "3"
    puts "Move"
    msg = CommsMessages::Movement.new
    msg.object = "me"
    msg.speed = 10
    msg.from = CommsMessages::Coords.new
    printf("FromX:")
    msg.from.x = Integer(gets.chomp)
    printf("FromY:")
    msg.from.y = Integer(gets.chomp)
    msg.to = CommsMessages::Coords.new
    printf("ToX:")
    msg.to.x = Integer(gets.chomp)
    printf("ToY:")
    msg.to.y = Integer(gets.chomp)
    bin = msg.to_s
    client.send("move|" + bin)
  when "4"
    puts "Exit"
    exit
  else
    puts "Unknown option"
  end
  #puts("Sent: %p\n", data)
end
puts("Client closing")
client.close()

#rescue Exception => e
# puts "Exception #{e}"
#end
