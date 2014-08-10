defmodule Webserver.Toppage.Handler do

def init(_transport, _req, []) do
	# For the random number generator:
  :random.seed(:erlang.now)
	{:upgrade, :protocol, :cowboy_rest}
end

def allowed_methods(req, state) do
  {["GET", "POST"], req, state}
end

def content_types_accepted(req, state) do
	{
   [ {{"application", "x-www-form-urlencoded", []}, :create_paste} ], Req, State
  }
end

def content_types_provided(req, state) do
  IO.puts "Toppage.Handler called"
	{[
		{"text/html", :hello_to_html},
		{"application/json", :hello_to_json},
		{"text/plain", :hello_to_text}
	], req, state}
end

def create_paste(req, state) do
	pasteID = new_paste_id()
	{:ok, [{"paste", paste}], req3} = :cowboy_req.body_qs(req)
#	:ok = file.write_file(full_path(pasteID), paste)
#	case :cowboy_req.method(req3) of
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
