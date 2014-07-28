defmodule Handlers.Status do
#  @behaviour :cowboy_http_handler

  #def init({_any, :http}, req, []) do
  def init(_transport, _req, []) do
    #{:ok, req, :undefined}
    {:upgrade, :protocol, :cowboy_rest}
  end

  #def handle(req, state) do
    #IO.puts "Handlers.Status called!"
    ##{:ok, data} = File.read "assets/index.html"
    ##{:ok, req2} = :cowboy_req.reply 200, [], data, req
    #{:ok, req2} = :cowboy_req.reply 200,[{"content-type", "text/plain"}], "Hello World!", req
    ##{:ok, req2} = :cowboy_req.reply 200,[], "Hello World!", req
    #{:ok, req2, state}
  #end

  def content_types_provided(Req, State) do
    {[
      {"text/html", :hello_to_html},
      {"application/json", :hello_to_json},
      {"text/plain", :hello_to_text}
    ], Req, State}
  end

  def hello_to_html(Req, State) do
    Body = "
     <html>
      <head>
       <meta charset=\"utf-8\">
        <title>REST Hello World!</title>
      </head>
      <body>
       <p>REST Hello World as HTML!</p>
      </body>
     </html>"
    {Body, Req, State}
  end

  def hello_to_json(Req, State) do
    Body = "{\"rest\": \"Hello World!\"}"
    {Body, Req, State}
  end

  def hello_to_text(Req, State) do
    {"REST Hello World as text!", Req, State}
  end


  def terminate(_reason, _request, _state) do
    :ok
  end
end

