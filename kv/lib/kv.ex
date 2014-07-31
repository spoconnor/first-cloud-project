defmodule KV do
 use Application   # this module defined in mix.exs as the 'application callback module'

 def start(_type, _args) do
  KV.Supervisor.start_link
 end

end
