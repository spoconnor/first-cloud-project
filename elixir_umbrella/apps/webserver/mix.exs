defmodule Elixirwebserver.Mixfile do
  use Mix.Project

  def project do
    [app: :elixirwebserver,
     version: "0.0.1",
     #elixir: "~> 0.14.2-dev",
     escript: escript,
     deps: deps]
  end

  def escript do
    [main_module: Elixirwebserver,
    embed_extra_apps: [:mix] ]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [
      { :erl_opts, [parse_transform: "lager_transform"] }
    ]
  end


  # Dependencies can be hex.pm packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [
      #{:cowboy, github: "extend/cowboy"},
      #{:lager, github: "basho/lager" }
      {:cowboy, path: "cowboy"},
      {:lager, path: "lager" }
    ]
  end
end
