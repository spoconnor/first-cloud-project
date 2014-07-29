defmodule Erlskeletor_cowboy do
use Application

# @doc Stops the application
#def stop() do
#    :application.stop(Erlskeletor_cowboy)
#end

# behaviour
# @private
def start(_StartType, _StartArgs) do
    IO.puts "Ensure all started"
    :application.ensure_all_started(Erlskeletor_cowboy)

    IO.puts "Starting..."
    Erlskeletor_cowboy_sup.start_link()

    IO.gets "Press enter"
    :ok
end

# @private
#defp stop(_State) do
#    :ok
#end


end
