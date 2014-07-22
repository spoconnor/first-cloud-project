defmodule PhoenixWebserver.Router do
  use Phoenix.Router

  plug Plug.Static, at: "/static", from: :phoenix_webserver
  get "/", PhoenixWebserver.Controllers.Pages, :index, as: :page
end
