defmodule Webserver.Mixfile do
  use Mix.Project

  def project do
    [app: :Webserver,
     version: "0.0.1",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.0.0",
     deps: deps]
  end

  def application do
    # Starting :inets and :crypto first, or it will not start cowboy
    [applications:
[:kernel, :stdlib, :lager, :ranch, :cowlib, :cowboy, :jiffy, :ssl, :ibrowse, :inets, :crypto, :riakc ],
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
      {:riakc, github: "basho/riak-erlang-client"},
      #{:riakclient, github: "drewkerrigan/riak-elixir-client" },
      {:eper, github: "massemanet/eper" },
      {:mixer, github: "opscode/mixer" },
      {:sync, github: "rustyio/sync" },
      #{:'riak-elixir-client', github: "drewkerrigan/riak-elixir-client" }
      {:riakc, github: "basho/riak-erlang-client" },
      {:exprotobuf, github: "bitwalker/exprotobuf", tag: "0.8.3"},
      {:gpb, github: "tomas-abrahamsson/gpb", tag: "3.16.0", override: :true},
      #{:amqp_client, github: "mochi/amqp_client", override: :true},
      #{:amqp_client, github: "jbrisbin/amqp_client" },
      {:amqp, github: "pma/amqp", tag: "v0.0.6" },
      #{:rabbit_common, github: "mochi/rabbit_common", override: :true, compile: :false, app: :false},
      #{:meck, github: "eproxus/meck"},
      #{:meck, github: "basho/meck", tag: "0.8.1p1", override: :true},
      #{:gen_bunny, github: "mochi/gen_bunny"},
      #{:exrabbit, github: "d0rc/exrabbit"},
      #{:erm, github: "k1complete/erm"},
    ]
  end

end
