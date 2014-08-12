defmodule Webserver.RestApi.Handler do

#POST
#
#curl -vX POST http://localhost:8080/news \
#-H"Content-Type:application/json" \
#-d'{ "title": "The Title", "content": "The Content" }'
#
#GET
#
#curl -vX GET http://localhost:8080/news

def init(_transport, req, []) do
	# For the random number generator:
  :random.seed(:erlang.now)

  # Specify handler based on request method
  case :cowboy_req.method(req) do
    {"POST", _} ->
	    {:upgrade, :protocol, :cowboy_rest}
    {"GET", req1} ->
      handle_get(req1)
  end
end

def allowed_methods(req, state) do
  #{["GET", "POST"], req, state}
  # Only allow Post requests
  {["POST"], req, state}
end

def content_types_accepted(req, state) do
	{
   #[ {{"application", "x-www-form-urlencoded", []}, :create_paste} ], Req, State
   # Only application/json is accepted
   # parsed using handle_post/2
   [ {{"application", "json", []}, :handle_post} ], Req, State
  }
end

# Resource exists defaults to true so we need to change it
# since every POST should create a new event
def resource_exists(req, state) do
  {:false, req, state}
end

def handle_post(req, state) do
  # Get the request body
  {:ok, body, req1} = :cowboy_req.body(req)

  # Decode it as JSON
  case :jiffy.json_decode(body) do
    {params} ->
      # Extract known properties, using defaults if needed
      title = :proplists.get_value("title", params, "news")
      content = :proplists.get_value("content", params, "")

      # Save it to database
      #data = db....

      # Send notification to listeners
      #notify(data)

      # Return 204 no content
      {:true, req1, state}

    {:bad_json, reason} ->
      # return 400 with the json encoded error
      {:ok, req2} = :cowboy_req.reply(400, [], :jiffy.encode(reason), req1)
      {:halt, req2, state}
  end
end

#def notify(data) do
# :lists.foreach(
#   fn(listener) ->
#     listener ! {:data, data}
#   end
#   pg2.get_members(data_listeners))
#end

def handle_get(req) do
  # set the response encoding properly for SSE
  {:ok, req1} = :cowboy_req.chunked_reply(
    200, 
    [{"content-type", "text/event-stream"}], req)

  # get latest data from database
  datalist = {"blah...", "test", "wibble"}

  # send each in the response
  :lists.foreach(
    fn(data) ->
      send_data(data, req1)
    end, datalist)

  # Add to listeners group
  #:ok = pg2.join(data_listeners, self())

  # Instruct cowboy to start looping and hibernate until a message is recv
  {:loop, req1, :undefined, :hibernate}
end

# called on every message received by hadler
def info({:data, data}, req, state) do
  # send data to listener
  send_data(data, req)
  # keep looping
  {:loop, req, state, :hibernate}
end

def send_data(data, req) do
 #todo
 :ok
end


##### not needed...

#def content_types_provided(req, state) do
#  IO.puts "Toppage.Handler called"
#	{[
#		{"text/html", :hello_to_html},
#		{"application/json", :hello_to_json},
#		{"text/plain", :hello_to_text}
#	], req, state}
#end

def create_paste(req, state) do
	pasteID = new_paste_id()
	{:ok, [{"paste", paste}], req3} = :cowboy_req.body_qs(req)
#	:ok = file.write_file(full_path(pasteID), paste)
#	case :cowboy_req.method(req3) do
#		{"POST", req4} ->
#			{{:true, <<$/, PasteID/binary>>}, req4, state};
#		{_, Req4} ->
#			{:true, req4, state}
#	end
end

def hello_to_html(req, state) do
  IO.puts "hello_to_html called"
  IO.puts "req:#{req}"
  IO.puts "state:#{state}"
	body = "
<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Hello World!</title>
</head>
<body>
	<p>REST Hello World as HTML!</p>
</body>
</html>"

	{body, req, state}
end

def hello_to_json(req, state) do
  IO.puts "hello_to_json called"
	body = "{\"rest\": \"Hello World!\"}"
	{body, req, state}
end

def hello_to_text(req, state) do
  IO.puts "hello_to_text called"
	{"REST Hello World as text!", req, state}
end



def new_paste_id() do
  "123"
#	initial = random.uniform(62) - 1
#	new_paste_id(initial, 7)
end
#def new_paste_id(bin, 0) do
#	chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
#	<< <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= bin >>
#end
#new_paste_id(bin, rem) do
#	next = random:uniform(62) - 1
#	new_paste_id(<<bin/binary, next>>, rem - 1)
#end

end
