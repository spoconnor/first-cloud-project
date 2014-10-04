require 'rubygems'
require 'bundler/setup'
require 'faye/websocket'
require 'eventmachine'

port   = ARGV[0] || 8081
secure = ARGV[1] == 'ssl'

EM.run {
  scheme  = secure ? 'wss' : 'ws'
  url     = "#{scheme}://localhost:#{port}/"
  headers = {'Origin' => 'http://faye.jcoglan.com'}
  ws      = Faye::WebSocket::Client.new(url, nil, :headers => headers)

  puts "Connecting to #{ws.url}"

  ws.onopen = lambda do |event|
    p [:open]
    puts "Sending Message..."
    ws.send("Hello World!!!\r\n")
    ws.send("Hello World!!!")
    ws.send("Hello World!!!")
  end

  ws.onmessage = lambda do |event|
    p [:message, event.data]
    # ws.close 1002, 'Going away'
  end

  ws.onclose = lambda do |event|
    p [:close, event.code, event.reason]
    EM.stop
  end
}