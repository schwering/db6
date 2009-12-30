package This_Computer
is
   DEVICE_NAME : constant String := "NONONO";
   BTREE_DEV   : constant String := DEVICE_NAME & "/btree";
   HEAP_DEV    : constant string := DEVICE_NAME & "/hipi";

   BASE_DIR    : constant String := ".tmp/";
   BTREE_FILE  : constant String := BASE_DIR & "btree";
   HEAP_FILE   : constant string := BASE_DIR & "heap";
end This_Computer;

