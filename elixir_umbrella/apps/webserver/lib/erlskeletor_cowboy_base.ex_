# @doc Handler for / endpoints
defmodule Erlskeletor_cowboy_base do

# cowboy
def init(_Transport, _Req, _Opts) do
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


end
