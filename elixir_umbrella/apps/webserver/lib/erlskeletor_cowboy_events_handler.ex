defmodule Erlskeletor_cowboy_events_handler do

def init(_Transport, Req, _Opts) do
    Headers = [{"content-type", "text/event-stream"}]
    {:ok, Req2} = :cowboy_req.chunked_reply(200, Headers, Req)
    :erlang.send_after(1000, self(), {:message, human_readable_date()})
    {:loop, Req2, :undefined}
end

def terminate(_Reason, _Req, _State) do
    :ok
end

def info({message, Msg}, Req, State) do
    :ok = :cowboy_req.chunk(["id: date", "\ndata: ", Msg, "\n\n"], Req)
    :erlang.send_after(1000, self(), {message, human_readable_date()})
    {:loop, Req, State}
end

# internal
defp human_readable_date() do
    TimeStamp = :os.timestamp()
    {{Year, Month, Day}, {Hour, Minute, Second}} = :calendar.now_to_universal_time(TimeStamp)
    DateList = :io_lib.format("~p-~p-~pT~p:~p:~pZ", [Year, Month, Day, Hour, Minute, Second])
    DateList
end

end
