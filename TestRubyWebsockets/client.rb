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
objectid = 0

  PING = 1;
  REGISTER = 2;
  REGISTERED = 3;
  SAY = 4;
  MOVEMENT = 5;
  ACTION = 6;
  BLOCK = 7;

Thread.new() do
  while data = client.receive()
    printf("Received [%p]\n", data)
    header = Header.new
    header.parse_from_string(data[0..1])
    case header.msgtype
    when REGISTERED
      puts("Registered")
      msg = Registered.new
      msg.parse_from_string(data[2..9999])
      objectid = msg.objectid
      puts("id: #{objectid}")
      puts("Motd: #{msg.motd}")
    when SAY
      puts("Say")
      msg = Say.new
      msg.parse_from_string(data[2..9999])
      puts("From: #{msg.from}")
      puts("Target: #{msg.target}")
      puts("Say: #{msg.text}")
    else
      puts("Unknown")
    end
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
  header = Header.new
  case selection
  when "1"
    puts "Register"
    printf("Name:")
    header.msgtype = REGISTER
    msg = Register.new
    msg.name = gets.chomp
  when "2"
    puts "Say"
    printf("Message:")
    header.msgtype = SAY
    msg = Say.new
    msg.from = objectid
    msg.target = 999
    msg.text = gets.chomp
  when "3"
    puts "Move"
    header.msgtype = MOVE
    msg = Move.new
    msg.object = objectid
    msg.speed = 10
    msg.from = Coords.new
    printf("FromX:")
    msg.from.x = Integer(gets.chomp)
    printf("FromY:")
    msg.from.y = Integer(gets.chomp)
    msg.to = Coords.new
    printf("ToX:")
    msg.to.x = Integer(gets.chomp)
    printf("ToY:")
    msg.to.y = Integer(gets.chomp)
  when "4"
    puts "Exit"
    exit
  else
    puts "Unknown option"
    header.msgtype = PING
    msg = Ping.new
    msg.count = 1
  end
  client.send(header.to_s + msg.to_s)
end
puts("Client closing")
client.close()

#rescue Exception => e
# puts "Exception #{e}"
#end
