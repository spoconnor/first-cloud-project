defmodule Erlskeletor_cowboy_foobar_handler do
#-include_lib("mixer/include/mixer.hrl").

#-mixin([
#        {Erlskeletor_cowboy_base,
#         [
#          init/3,
#          allowed_methods/2,
#          content_types_provided/2,
#          is_authorized/2
#         ]
#        }
#       ]).

def handle_get(Req, State) do
    Body = :jiffy.encode({[{:foo, :bar}]})
    {Body, Req, State}
end

end
