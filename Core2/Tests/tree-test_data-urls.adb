with DB.Types;
with DB.Types.Keys;
with DB.Types.Times;

with DB.Types.Gen_Strings;
with DB.Types.Strings;
with DB.Types.Values;

pragma Warnings (Off);
with DB.Types.Gen_Strings.Gen_Bounded;
pragma Warnings (On);

pragma Warnings (Off);
with DB.Types.Gen_Strings.Gen_Unbounded;
pragma Warnings (On);

with DB.Locks.Mutexes;

with Ada.Unchecked_Conversion;

package body Tree.Test_Data.URLs is

   Mutex : DB.Locks.Mutexes.Mutex_Type;

   generic
      with package Strings is new DB.Types.Gen_Strings(Tree.Types.Char_Type);
      type String_Type is private;
      with function New_String (Buf : Strings.Indefinite_Buffer_Type)
         return String_Type is <>;
      with function "&" (S, T : String_Type) return String_Type is <>;
   package String_Makers is
      function Make_String (Index : Index_Type) return String_Type;
   end String_Makers;

   package body String_Makers is
      TLDs : constant array (Index_Type range <>) of String_Type :=
         (New_String("com"),
          New_String("org"),
          New_String("net"),
          New_String("edu"), 
          New_String("gov"), 
          New_String("info"), 
          New_String("name"), 
          New_String("biz"), 
          New_String("pro"), 
          New_String("eu"), 
          New_String("localhost"), 
          New_String("local"), 
          New_String("aero"), 
          New_String("arpa"), 
          New_String("asia"), 
          New_String("coop"), 
          New_String("int"), 
          New_String("jobs"), 
          New_String("mil"), 
          New_String("mobi"), 
          New_String("museum"), 
          New_String("tel"), 
          New_String("travel"), 
          New_String("post"), 
          New_String("xxx"), 
          New_String("de"), 
          New_String("ch"), 
          New_String("ac"), 
          New_String("li"), 
          New_String("nl"), 
          New_String("cn"), 
          New_String("jp"), 
          New_String("fr"), 
          New_String("us"), 
          New_String("uk.co")); 
      Domains : constant array (Index_Type range <>) of String_Type :=
         (New_String("ibm"),
          New_String("hotmail"),
          New_String("google"),
          New_String("microsoft"), 
          New_String("bundesarchiv"), 
          New_String("videolan"), 
          New_String("bild"), 
          New_String("faz"), 
          New_String("sueddeutsche"), 
          New_String("heise"), 
          New_String("faz"), 
          New_String("plus"), 
          New_String("mplayer"), 
          New_String("wikipedia"), 
          New_String("faz"), 
          New_String("rathaus"), 
          New_String("dom"), 
          New_String("gaffel"), 
          New_String("frueh"), 
          New_String("koelsch"), 
          New_String("bitburger"));
      Subdomains : constant array (Index_Type range <>) of String_Type :=
         (New_String("public"),
          New_String("informatik"),
          New_String("www"),
          New_String("www-i1"),
          New_String("www-i2"),
          New_String("www-i3"),
          New_String("www-i4"),
          New_String("www-i5"),
          New_String("www-i6"),
          New_String("www-i1"),
          New_String("www-i2"),
          New_String("www-i3"),
          New_String("www-i4"),
          New_String("www-i5"),
          New_String("www-i6"),
          New_String("www1"), 
          New_String("www2"), 
          New_String("www3"), 
          New_String("www4"), 
          New_String("www5"), 
          New_String("www6"), 
          New_String("www7"), 
          New_String("ftp"), 
          New_String("news"), 
          New_String("members"), 
          New_String("research"), 
          New_String("live"), 
          New_String("ticker"));
      Protocols : constant array (Index_Type range <>) of String_Type :=
         (New_String("http"),
          New_String("https"),
          New_String("ftp"),
          New_String("irc"),
          New_String("pop"),
          New_String("imap"),
          New_String("git"),
          New_String("svn"),
          New_String("svn+ssh"),
          New_String("ssh"),
          New_String("telnet"),
          New_String("smtp"));
      Paths : constant array (Index_Type range <>) of String_Type :=
         (New_String(""),
          New_String("/"),
          New_String("/index.html"),
          New_String("/index.htm"),
          New_String("/index.txt"),
          New_String("/index.py"),
          New_String("/default.asp"),
          New_String("/default.aspx"),
          New_String("/index.php"),
          New_String("/index.php3"),
          New_String("/index.php4"),
          New_String("/index.php5"),
          New_String("/home.html"),
          New_String("/home.htm"),
          New_String("/home.txt"),
          New_String("/home.py"),
          New_String("/home.php"),
          New_String("/home.php3"),
          New_String("/home.php4"),
          New_String("/news.html"),
          New_String("/news.htm"),
          New_String("/news.txt"),
          New_String("/news.py"),
          New_String("/news.php"),
          New_String("/news.php3"),
          New_String("/news.php4"),
          New_String("/news.php5"));

      function Make_String (Index : Index_Type) return String_Type
      is
         use type Types.Count_Type;
         TLD_Index : constant Index_Type :=
            Index mod TLDs'Length;
         Domain_Index : constant Index_Type :=
            (Index / TLDs'Length) mod Domains'Length;
         Subdomain_Index : constant Index_Type :=
            (Index / (TLDs'Length * Domains'Length)) mod Subdomains'Length;
         Protocol_Index : constant Index_Type :=
            (Index / (TLDs'Length * Domains'Length * Subdomains'Length)) mod
            Protocols'Length;
         Path_Index : constant Index_Type :=
            (Index / (TLDs'Length * Domains'Length * Subdomains'Length *
                      Protocols'Length)) mod Paths'Length;

         Dot : constant String_Type := New_String(".");
         Colon : constant String_Type := New_String(":");
      begin
         return TLDs(TLD_Index) & Dot &
                Domains(Domain_Index) & Dot &
                Subdomains(Subdomain_Index) & Colon &
                Protocols(Protocol_Index) &
                Paths(Path_Index);
      end Make_String;
   end String_Makers;


   package Row_Makers is new String_Makers
     (Strings     => DB.Types.Strings,
      String_Type => DB.Types.Keys.Rows.String_Type,
      New_String  => DB.Types.Keys.Rows.New_String,
      "&"         => DB.Types.Keys.Rows."&");

   package Column_Makers is new String_Makers
     (Strings     => DB.Types.Strings,
      String_Type => DB.Types.Keys.Rows.String_Type,
      New_String  => DB.Types.Keys.Rows.New_String,
      "&"         => DB.Types.Keys.Rows."&");

   function Make_Value1 (Count : Types.Count_Type) return Types.Value_Type
   is
      type Uint32 is mod 2**32;
      type Definite_Buffer_Type is
         new DB.Types.Values.Indefinite_Buffer_Type(1 .. 4);
      function Convert is new Ada.Unchecked_Conversion
        (Uint32, Definite_Buffer_Type);

      use type Types.Count_Type;
      I   : constant Uint32 
          := Uint32(Count mod Uint32'Modulus);
      Buf : constant DB.Types.Values.Indefinite_Buffer_Type
          := DB.Types.Values.Indefinite_Buffer_Type(Convert(I));
   begin
      return Types.New_Value(Buf);
   end Make_Value1;


   function Make_Value2 (Count : Types.Count_Type) return Types.Value_Type
   is
      Max_Len : constant := 4;
      Img  : constant String   := Types.Count_Type'Image(Count);
      From : constant Positive := Integer'Max(Img'Last - Max_Len+1, Img'First);
      Sub  : constant String   := Img(From .. Img'Last);

      subtype R is Positive range From .. Img'Last;
      type Definite_String_Type is new String(R);
      type Definite_Buffer_Type is
         new DB.Types.Values.Indefinite_Buffer_Type(R);
      function Convert is new Ada.Unchecked_Conversion
        (Definite_String_Type, Definite_Buffer_Type);

      Buf : constant DB.Types.Values.Indefinite_Buffer_Type
          := DB.Types.Values.Indefinite_Buffer_Type
                (Convert(Definite_String_Type(Sub)));
   begin
      return Types.New_Value(Buf);
   end Make_Value2;


   pragma Unreferenced (Make_Value1);
   function Make_Value (Count : Types.Count_Type) return Types.Value_Type
   renames Make_Value2;


   procedure Init_Key_Value_Pairs (Init : in Types.Count_Type) is
   begin
      DB.Locks.Mutexes.Lock(Mutex);
      Initial_KV := Init;
      Current_KV := Initial_KV;
      DB.Locks.Mutexes.Unlock(Mutex);
   end Init_Key_Value_Pairs;


   procedure Reset_String_Generation is
   begin
      DB.Locks.Mutexes.Lock(Mutex);
      Current_KV := Initial_KV;
      DB.Locks.Mutexes.Unlock(Mutex);
   end Reset_String_Generation;


   function Random_Entry return Types.Key_Value_Type
   is
      use type Types.Count_Type;
      KV : Types.Key_Value_Type;
      I  : Index_Type renames Current_KV;
   begin
      DB.Locks.Mutexes.Lock(Mutex);
      KV.Key.Row := Row_Makers.Make_String(I);
      KV.Key.Column := Column_Makers.Make_String(I*I);
      KV.Key.Time := DB.Types.Times.Number_Type(I);
      KV.Value := Make_Value(I);
      Current_KV := Current_KV + 1;
      DB.Locks.Mutexes.Unlock(Mutex);
      return KV;
   end Random_Entry;

end Tree.Test_Data.URLs;

