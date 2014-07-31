defmodule KVTest do
#  use ExUnit.Case
   use ExUnit.Case, async: true

  test "the truth" do
    assert 1 + 1 == 2
  end

# test a crash in bucket doesnt also crash registry

test "removes bucket on crash", %{registry: registry} do
  KV.Registry.create(registry, "shopping")
  {:ok, bucket} = KV.Registry.lookup(registry, "shopping")

  # Kill the bucket and wait for the notification
  Process.exit(bucket, :shutdown)
  assert_receive {:exit, "shopping", ^bucket}
  assert KV.Registry.lookup(registry, "shopping") == :error
end

end


defmodule KV.BucketTest do
use ExUnit.Case, async: true

# use async: true to let all test run in parrallel

 setup do
  {:ok, bucket} = KV.Bucket.start_link
  {:ok, bucket: bucket}   # Pass pid back via the test context
 end

 test "stores values by key", %{bucket: bucket} do
  # 'bucket' is now available inside test
  assert KV.Bucket.get(bucket, "milk") == nil

  KV.Bucket.put(bucket, "milk", 3)
  assert KV.Bucket.get(bucket, "milk") == 3
 end

end

 

defmodule KV.RegistryTest do
  use ExUnit.Case

  setup do
    {:ok, registry} = KV.Registry.start_link
    {:ok, registry: registry}
  end

  test "spawns buckets", %{registry: registry} do
    assert KV.Registry.lookup(registry, "shopping") == :error

    KV.Registry.create(registry, "shopping")
    assert {:ok, bucket} = KV.Registry.lookup(registry, "shopping")

    KV.Bucket.put(bucket, "milk", 1)
    assert KV.Bucket.get(bucket, "milk") == 1
  end

  test "removes buckets on exit", %{registry: registry} do
    KV.Registry.create(registry, "shopping")
    {:ok, bucket} = KV.Registry.lookup(registry, "shopping")
    Agent.stop(bucket)
    assert KV.Registry.lookup(registry, "shopping") == :error
   end

end



defmodule Forwarder do
  use ExUnit.Case
  use GenEvent

  def handle_event(event, parent) do
    send parent, event
    {:ok, parent}
  end
end

