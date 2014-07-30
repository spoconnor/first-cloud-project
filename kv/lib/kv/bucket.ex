defmodule KV.Bucket do

# Uses 'Agent'. Good for storing and retrieving simple state
#Interactive Elixir (0.14.2) - press Ctrl+C to exit (type h() ENTER for help)
#iex(1)> {:ok, bucket} = KV.Bucket.start_link
#{:ok, #PID<0.68.0>}
#iex(2)> KV.Bucket.put(bucket, :test, "Hello")
#:ok
#iex(3)> KV.Bucket.get(bucket, :test)         
#"Hello"

# in current version of elixir, Map is less efficient than HashDict

 @doc """
 Starts a new bucket
 """
 def start_link do
  # Agent.start_link returns {:ok, pid}
  Agent.start_link(fn -> HashDict.new end)
 end

 @doc """
 Gets a value from the bucket by key
 """
 def get(bucket, key) do
  Agent.get(bucket, &HashDict.get(&1, key))
 end

 @doc """
 Puts the value for the given key in the bucket
 """
 def put(bucket, key, value) do
  Agent.update(bucket, &HashDict.put(&1, key, value))
 end

 @doc """
 Deletes 'key' from bucket
 Returns the current value of key, if key present
 """
 def delete(bucket, key) do

  #Agent.get_and_update(bucket, &HashDict.pop(&1, key))
  # Same as...
  Agent.get_and_update(bucket, fn dict-> 
   HashDict.pop(&1, key))
  # Note. Part inside the fn executed on the server

 end

end
