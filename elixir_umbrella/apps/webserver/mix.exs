defmodule Webserver.Mixfile do
  use Mix.Project

  def project do
    [app: :Webserver,
     version: "0.0.1",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 0.14.2-dev",
     deps: deps]
  end

  def application do
    # Starting :inets and :crypto first, or it will not start cowboy
    [applications:
[:kernel, :stdlib, :lager, :ranch, :cowlib, :cowboy, :jiffy, :ssl, :ibrowse, :inets, :crypto],
     mod: {Webserver, []},
     env:  [
        http_port: 8080,
        http_listener_count: 10
      ],
    ]
  end

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
