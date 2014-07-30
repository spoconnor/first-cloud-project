defmodule Erlskeletor_cowboy_foobar_handler do

def init(_Transport, _Req, _Opts) do
  IO.puts "foobar handler init"
  {:upgrade, :protocol, :cowboy_rest}
end

def allowed_methods(Req, State) do
  {["GET"], Req, State}
end

def content_types_provided(Req, State) do
  {[{"application/json", :handle_get}], Req, State}
end

def is_authorized(Req, State) do
  {:true, Req, State}
end


def handle_get(Req, State) do
  IO.puts "foobar handle get"
  Body = :jiffy.encode({[{:foo, :bar}]})
  {Body, Req, State}
end

end


