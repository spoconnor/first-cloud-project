defmodule PhoenixWebserver do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    IO.puts "PhoenixWebServer.start"
    PhoenixWebserver.Supervisor.start_link
  end
end
