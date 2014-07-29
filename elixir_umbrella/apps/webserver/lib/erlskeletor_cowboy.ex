defmodule Erlskeletor_cowboy do
@behaviour :application


# application
# @doc Starts the application
def start() do
    :application.ensure_all_started(Erlskeletor_cowboy)
end

# @doc Stops the application
def stop() do
    :application.stop(Erlskeletor_cowboy)
end

# behaviour
# @private
def start(_StartType, _StartArgs) do
    Erlskeletor_cowboy_sup.start_link()
end

# @private
defp stop(_State) do
    :ok
end


end
