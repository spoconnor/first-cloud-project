defmodule Webserver.Database do

# Save a value
def SaveData(bucket, key, data) do
  u = RObj.create(bucket: bucket, key: key, data: data)
    |> Riak.put
end

# Find an object
def FindData(bucket, key) do
  u = Riak.find bucket, key
end

# Update an object
def UpdateData (obj, data) do
  obj = obj.data(data)
    |> Riak.put
end

end
