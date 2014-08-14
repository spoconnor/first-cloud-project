defmodule Webserver.Database do

# Save a value
def saveData(bucket, key, data) do
  IO.puts "Saving #{key}=#{data}"
  u = RObj.create([bucket: bucket, key: key, data: data])
    |> Riak.put
end

# Find an object
def findData(bucket, key) do
  IO.puts "Finding #{key}"
  u = Riak.find bucket, key
end

# Update an object
def updateData(obj, data) do
  IO.puts "Updating #{data}"
  obj = obj.data(data)
    |> Riak.put
end

end
