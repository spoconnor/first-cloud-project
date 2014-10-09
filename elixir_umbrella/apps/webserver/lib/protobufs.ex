defmodule Messages do
  use Protobuf, from: Path.expand("../../../../ProtoBufs/webservice.proto", __DIR__)
end
