defmodule PhoenixWebserver.Mixfile do
  use Mix.Project

  def project do
    [ app: :phoenix_webserver,
      version: "0.0.1",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 0.14.2",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { PhoenixWebserver, [] },
      applications: [:phoenix]
    ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat" }
  defp deps do
    [
      {:phoenix, "0.3.0"},
      {:cowboy, github: "extend/cowboy"}
      #{:cowboy, "~> 0.10.0", github: "extend/cowboy", optional: true}
    ]
  end
end