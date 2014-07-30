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

 
