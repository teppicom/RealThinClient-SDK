{
  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
  
  This unit is NOT used by the RealThinClient SDK nor by any RTC SDK examples!

  This is only an example DB Pool class implementation, written by Gerhard Knapp
  using IB*Objects to work with Firebird. Before you start using the code,
  you should take a couple of minutes to read what this is all about.

  In general, if you have a multithreaded server, you need a “pool” from which you
  will request a database connection when you need one, the return it when you don’t
  need it anymore. This pool has to create a new DB connection if there is no unused connection
  available and use critical sections to make sure that all threads will be able to access its methods.

  You can start by using a TList to act as your “pool”. You will need to implement 2 methods,
  I will call them “getDBConn” and “putDBConn”. “getDBConn” to “get” a DB connection object and
  “putDBCon(con:TDatabase)” to return unused connection object back into your pool.

  In “getDBCon”, first check if TList is empty. If Empty, create a new connection and add it to TList.
  Then, just take the last object from list (delete it from list) and return it as a result. The list
  will have at least one object in it, since you have created this in case it was empty before.
  In “putDBConn”, simply add the object back into TList.

  The only important thing here is that you use one critical section to secure both functions, so that
  you can securely work with the pool from all threads. To do this, you will use one critical section
  inside the get and the put method, like this …

  CS.Enter;
  try
     // your code
  finally
    CS.Leave;
    end;

  Then, inside each RTC method you will call getDBObj to obtain a database connection object and
  afterwards call putDBObj to return it back to your database connection pool. Also use try/finally
  to make sure that object will be returned to the pool, even if exception should be raised somewhere
  inside your code, like this …

  myDB := getDBConn;
  try
    // work with “myDB” database connection
  finally
    putDBConn(myDB);
    end;

  Well, that’s all there is to database connection pooling. You will need one TList, one TRtcCritSec and
  implement one “get” and one “put” method. You can enhance this implementation by adding a second list,
  where you will maintain a list of used database objects, so you can close them when your application
  closes, even if they weren’t returned back to the pool. But, that is not really necessary if you keep
  true to the simple rule: getDBConn has to be followed by putDBConn in a try/finally block.

  Since the best way to learn is by example, here is a Database Pool class example. It won’t compile if you
  don’t have IB*Objects installed, but it should show you how you can implement your own Database pooling …
}

unit rtcIBODBPool;

interface

uses
  Classes,
  SysUtils,

  rtcSystem,

  DB, IB_Session,
  IBODataset, IB_Components;

type
  TDBPool = class
  private
    CS:TRtcCritSec;
    MyPool:TList;

    { Create, Set-up and Open a new database connection }
    function SetUpDB:TIBODatabase;

  public
    { Database parameters:
      Set before first call to AddDBConn or GetDBConn. }
    db_server,
    db_path,
    db_charset,
    db_username,
    db_password:string;

    constructor Create;
    destructor Destroy; override;

    { Create/Open a new DB connection and add it to this
      database pool (useful for pool initialization). }
    procedure AddDBConn;

    { Get one Database connection out of the Pool.
      This will create a new connection if pool is empty. }
    function GetDBConn:TIBODatabase;

    { Put a database connection back into the pool.
      Need to call this after you’re done using the connection. }
    procedure PutDBConn(conn:TIBODatabase);

    { Close all connections inside the Pool. }
    procedure CloseAllDBConns;
  end;

implementation

constructor TDBPool.Create;
  begin
  inherited Create;
  CS:=TRtcCritSec.Create;
  MyPool:=TList.Create;
  { You can do more of your Pool and Database initialisation
     here and … don’t forgett to set the IB_GDS32 variable
     to point to the location of ‘fbclient.dll’ when using Firebird. }
  end;

destructor TDBPool.Destroy;
  begin
  CloseAllDBConns;
  // Do more of your Pool and DB finalization here
  MyPool.Free;
  CS.Free;
  inherited;
  end;

function TDBPool.SetUpDB:TIBODatabase;
  begin
  Result:= TIBODatabase.Create(nil);
  try
    with Result do
      begin
      Server:= db_server;
      Path:= db_path;
      CharSet:= db_charset;
      Username:= db_UserName;
      Password:= db_Password;
      Protocol:= cpTCP_IP;
      keepconnection:= true;
      // Do all your Connection initialization here.
      connect;
    end;
  except
    Result.Free;
    Result:=nil;
    raise; // re-raise the exception
  end;
end;

procedure TDBPool.AddDBConn;
  begin
  CS.Enter;
  try
    myPool.Add(SetUpDB);
  finally
    CS.Leave;
    end;
  end;

function TDBPool.GetDBConn:TIBODatabase;
  begin
  Result:=nil;
  CS.Enter;
  try
    if myPool.Count > 0 then
      begin
      Result:= myPool.items[myPool.Count-1];
      myPool.Delete(myPool.Count-1);
      end;
  finally
    CS.Leave;
    end;
  { Now we either have the connection,
     or we need to create one. }
  if Result=nil then
    Result:=SetupDB
  else if not Result.VerifyConnection then
   { connection was dropped/closed in the meantime }
    begin
    Result.Free;
    Result:= SetupDB;
    end;
  end;

procedure TDBPool.PutDBConn(conn:TIBODatabase);
  begin
  CS.Enter;
  try
    mypool.Add(conn);
  finally
    CS.Leave;
    end;
  end;

procedure TDBPool.CloseAllDBConns;
  var
    i    :integer;
    dbx  :TiboDatabase;
  begin
  CS.Enter;
  try
    for i:= 0 to mypool.count - 1 do begin
      dbx:= myPool.items[i];
      // Do all your connection finalization here.
      dbx.ForceDisconnect;
      dbx.free;
      dbx:= nil;
    end;
    myPool.clear;
  finally
    CS.Leave;
    end;
  end;

end.
