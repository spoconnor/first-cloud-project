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

Thread.new() do
  while data = client.receive()
    printf("Received [%p]\n", data)
    msg = Base.new
    msg.parse_from_string(data)
    case msg.msgtype
    when Base::MsgType::ERegistered
      puts("Registered")
      objectid = msg.registered.objectid
      puts("id: #{objectid}")
      puts("Motd: #{msg.registered.motd}")
    when Base::MsgType::ESay
      puts("Say")
      puts("From: #{msg.say.from}")
      puts("Target: #{msg.say.target}")
      puts("Say: #{msg.say.text}")
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
  msg = Base.new
  case selection
  when "1"
    puts "Register"
    printf("Name:")
    msg.msgtype = Base::MsgType::ERegister
    msg.register = Base::Register.new
    msg.register.name = gets.chomp
  when "2"
    puts "Say"
    printf("Message:")
    msg.msgtype = Base::MsgType::ESay
    msg.say = Base::Say.new
    msg.say.from = objectid
    msg.say.target = 999
    msg.say.text = gets.chomp
  when "3"
    puts "Move"
    msg.msgtype = Base::MsgType::EMove
    msg.move = Base::Move.new
    msg.move.object = objectid
    msg.move.speed = 10
    msg.move.from = Base::Coords.new
    printf("FromX:")
    msg.movement.from.x = Integer(gets.chomp)
    printf("FromY:")
    msg.movement.from.y = Integer(gets.chomp)
    msg.movement.to = Base::Coords.new
    printf("ToX:")
    msg.movement.to.x = Integer(gets.chomp)
    printf("ToY:")
    msg.movement.to.y = Integer(gets.chomp)
  when "4"
    puts "Exit"
    exit
  else
    puts "Unknown option"
    msg.msgtype = Base::MsgType::EPing
    msg.ping = Base::Ping.new
    msg.ping.count = 1
  end
  client.send(msg.to_s)

  puts("Test encode")
  a = msg.to_s
  puts("Test decode")
  b = Base.new
  b.parse_from_string(a)
  puts("#{b.say.text}")
  #puts("Sent: %p\n", data)
end
puts("Client closing")
client.close()

#rescue Exception => e
# puts "Exception #{e}"
#end
