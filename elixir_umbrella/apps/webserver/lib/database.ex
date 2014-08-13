defmodule Webserver.Database do

# Save a value
def saveData(bucket, key, data) do
  u = RObj.create([bucket: bucket, key: key, data: data])
    |> Riak.put
end

# Find an object
def findData(bucket, key) do
  u = Riak.find bucket, key
end

# Update an object
def updateData(obj, data) do
  obj = obj.data(data)
    |> Riak.put
end

end
