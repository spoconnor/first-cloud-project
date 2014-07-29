defmodule WebServer.Mixfile do
  use Mix.Project

  def project do
    [app: :Erlskeletor_cowboy,
     version: "0.0.1",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 0.14.2-dev",
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    # Starting :inets and :crypto first, or it will not start cowboy
    [applications: [:kernel, :stdlib, :lager, :cowboy, :jiffy, :ssl, :ibrowse, :inets, :crypto],
     mod: {Erlskeletor_cowboy, []},
     env:  [
        http_port: 8080,
        http_listener_count: 10
      ],
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
  # To depend on another app inside the umbrella:
  #
  #   {:myapp, in_umbrella: true}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [
      {:cowboy, github: "extend/cowboy"},
      {:lager, github: "basho/lager" },
      {:jiffy, github: "davisp/jiffy" },
      {:ibrowse, github: "cmullaparthi/ibrowse" },
      {:eper, github: "massemanet/eper" },
      {:mixer, github: "opscode/mixer" },
      {:sync, github: "rustyio/sync" }
    ]
  end

end
