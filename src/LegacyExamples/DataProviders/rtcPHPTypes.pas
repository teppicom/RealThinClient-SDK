{
  "PHP Scripting Type definition"
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br>)

  @longcode(#
  Based on PHP 4 Delphi
    by Serhiy Perevoznyk
       <serge_perevoznyk(at)hotmail.com>
       <http://users.chello.be/ws36637> #)

  @exclude
}
unit rtcPHPTypes;

{$INCLUDE rtcDefs.inc}

interface

{ If you are using PHP 4.0, no "PHP" compiler directives are needed.
  Otherwise, the following compiler directive is required:

  DEFINE PHP5 for PHP 5.0.x (seems to work with PHP 5.0.3)
  DEFINE PHP510 for PHP 5.1.x
  DEFINE PHP520 for PHP 5.2.x (seems to work with PHP 5.2.17)
  DEFINE PHP530 for PHP 5.3.x

  NOTE: This unit and PHP support from RTC is purely experimental.
  You should NOT use this code in a productive environment! }

{$A+}


{$IFDEF PHP530} {$DEFINE PHP520} {$ENDIF}
{$IFDEF PHP520} {$DEFINE PHP510} {$ENDIF}
{$IFDEF PHP510} {$DEFINE PHP5} {$ENDIF}

uses
  Windows,
  SysUtils,

  rtcThrPool;

const
  ZEND_HANDLE_FILENAME = 0;

  TSRM_ERROR_LEVEL_ERROR = 1;
  TSRM_ERROR_LEVEL_CORE = 2;
  TSRM_ERROR_LEVEL_INFO = 3;

type
  error_handling_t = (EH_NORMAL, EH_SUPPRESS, EH_THROW);

  size_t = cardinal;
  pzval = pointer;

  ppointer = ^pointer;
  pppointer = ^ppointer;
  zend_bool   = boolean;

  Psapi_module_struct = ^Tsapi_module_struct;

  TModuleShutdownFunc = function (globals : pointer) : Integer; cdecl;
  TModuleStartupFunc  = function (sapi_module : psapi_module_struct) : integer; cdecl;

  sapi_module_struct =  record
      name : PAnsiChar;
      pretty_name : PAnsiChar;
      startup  : TModuleStartupFunc;   //int (*startup)(struct _sapi_module_struct *sapi_module);
      shutdown : TModuleShutdownFunc;   //int (*shutdown)(struct _sapi_module_struct *sapi_module);
      activate : pointer;
      deactivate : pointer;
      ub_write : pointer;
      flush : pointer;
      stat : pointer;
      getenv : pointer;
      sapi_error : pointer;
      header_handler : pointer;
      send_headers : pointer;
      send_header : pointer;
      read_post : pointer;
      read_cookies : pointer;
      register_server_variables : pointer;
      log_message : pointer;
      {$IFDEF PHP5}
      {$IFDEF PHP510}
      get_request_time : pointer;
      {$ENDIF}
      {$IFDEF PHP530}
      terminate_process : pointer;
      {$ENDIF}
      {$ENDIF}
      php_ini_path_override : PAnsiChar;
      block_interruptions : pointer;
      unblock_interruptions : pointer;
      default_post_reader : pointer;
      treat_data : pointer;
      executable_location : PAnsiChar;
      php_ini_ignore : Integer;
      {******************************}
      {IMPORTANT:                    }
      {Please check your php version }
      {******************************}
      {$IFDEF PHP4}
      {$IFDEF PHP433}
      get_fd : pointer;
      force_http_10 : pointer;
      get_target_uid : pointer;
      get_target_gid : pointer;
      ini_defaults : pointer;
      phpinfo_as_text : integer;
      {$ENDIF}
      {$ENDIF}
      {$IFDEF PHP5}
      get_fd : pointer;
      force_http_10 : pointer;
      get_target_uid : pointer;
      get_target_gid : pointer;
      input_filter : pointer;
      ini_defaults : pointer;
      phpinfo_as_text : integer;
      {$IFDEF PHP520}
      ini_entries : PAnsiChar;
      {$ENDIF}
      {$IFDEF PHP530}
      additional_functions: Pointer;
      input_filter_init : pointer;
      {$ENDIF}
      {$ENDIF}
    end;
  Tsapi_module_struct = sapi_module_struct;

  Psapi_post_entry =  ^Tsapi_post_entry;
  sapi_post_entry =
    record
      content_type : PAnsiChar;
      content_type_len : uint;
      post_reader : pointer;  //void (*post_reader)(TSRMLS_D);
      post_handler : pointer; //void (*post_handler)(char *content_type_dup, void *arg TSRMLS_DC);
    end;
  Tsapi_post_entry = sapi_post_entry;

  Psapi_request_info = ^Tsapi_request_info;
  sapi_request_info =
    record
      request_method : PAnsiChar;
      query_string   : PAnsiChar;
      post_data      : PAnsiChar;
      raw_post_data  : PAnsiChar;
      cookie_data    : PAnsiChar;
      content_length : Longint;
      post_data_length : uint;
      raw_post_data_length : uint;
      path_translated : PAnsiChar;
      request_uri     : PAnsiChar;
      content_type    : PAnsiChar;
      headers_only    : zend_bool;
      no_headers      : zend_bool;
      {$IFDEF PHP5}
      headers_read    : zend_bool;
      {$ENDIF}
      post_entry      : PSapi_post_entry;
      content_type_dup : PAnsiChar;
      //for HTTP authentication
      auth_user : PAnsiChar;
      auth_password : PAnsiChar;
      {$IFDEF PHP510}
      auth_digest : PAnsiChar;
      {$ENDIF}
      //this is necessary for the CGI SAPI module
      argv0 : PAnsiChar;
      //this is necessary for Safe Mode
      current_user : PAnsiChar;
      current_user_length : Integer;
      //this is necessary for CLI module
      argc : Integer;
      argv : ^PAnsiChar;
      {$IFDEF PHP510}
      proto_num : integer;
      {$ENDIF}
    end;
   Tsapi_request_info = sapi_request_info;

  pzend_list_element = ^zend_list_element;
  zend_list_element = record
    prev: pzend_list_element;
    next: pzend_list_element;
    data: AnsiChar;
  end;

  pzend_llist = ^zend_llist;
  zend_llist = record
    head: pzend_list_element;
    tail: pzend_list_element;
    size: size_t;
    count: size_t;
    dtor: pointer;
    persistent: byte;
    traverse_ptr: pzend_list_element;
  end;

  Psapi_headers_struct = ^Tsapi_headers_struct;
  sapi_headers_struct =
    record
      headers : zend_llist;
      http_response_code : Integer;
      send_default_content_type : Byte;
      mimetype : PAnsiChar;
      http_status_line : PAnsiChar;
    end;
   Tsapi_headers_struct = sapi_headers_struct;

  PStat = ^TStat;
  TStat = record
    st_dev: Word;
    st_ino: Word;
    st_mode: Word;
    st_nlink: SmallInt;
    st_uid: SmallInt;
    st_gid: SmallInt;
    st_rdev: Word;
    st_size: Longint;
    st_atime: Longint;
    st_mtime: Longint;
    st_ctime: Longint;
  end;
  Stat = TStat;

  PBucket = ^TBucket;
  TBucket = record
    h: ulong;
    nKeyLength: uint;
    pData: Pointer;
    pDataPtr: Pointer;
    pListNext: PBucket;
    pListLast: PBucket;
    pNext: PBucket;
    pLast: PBucket;
    arKey: array[0..0] of AnsiChar;
  end;

  PHashTable = ^THashTable;
  THashTable =
    record
    nTableSize: uint;
    nTableMask: uint;
    nNumOfElements: uint;
    nNextFreeElement: ulong;
    pInternalPointer: PBucket;
    pListHead: PBucket;
    pListTail: PBucket;
    arBuckets: ^PBucket;
    pDestructor: pointer;
    persistent: boolean;
    nApplyCount: Byte;
    bApplyProtection: boolean;
  end;

  Psapi_globals_struct = ^Tsapi_globals_struct;
  _sapi_globals_struct =
    record
      server_context : Pointer;
      request_info : sapi_request_info;
      sapi_headers : sapi_headers_struct;
      read_post_bytes : Integer;
      headers_sent : Byte;
      global_stat : stat;
      default_mimetype : PAnsiChar;
      default_charset : PAnsiChar;
      rfc1867_uploaded_files : PHashTable;
      post_max_size : Longint;
      options : Integer;
      {$IFDEF PHP5}
      sapi_started : zend_bool;
      {$IFDEF PHP510}
      global_request_time : longint;
      known_post_content_types : THashTable;
      {$ENDIF}
      {$ENDIF}
    end;
  Tsapi_globals_struct = _sapi_globals_struct;

  Psapi_header_struct = ^Tsapi_header_struct;
  sapi_header_struct =
    record
      header : PAnsiChar;
      header_len : uint;
      {$IFNDEF PHP530}
      replace : zend_bool;
      {$ENDIF}
    end;
  Tsapi_header_struct = sapi_header_struct;

{$IFDEF PHP5}
   zend_stream_reader_t = function(handle : pointer; buf : PAnsiChar; len : size_t; TSRMLS_DC : pointer) : size_t; cdecl;
   zend_strem_closer_t = procedure(handle : pointer; TSRMLS_DC : pointer); cdecl;

  _zend_stream = record
   handle : pointer;
   reader : pointer;
   closer : pointer;
   interactive : pointer;
   end;
   TZendStream = _zend_stream;
   PZendStream = ^TZendStream;
{$ENDIF}

  zend_file_handle =
    record
    _type: uchar;
    filename: PAnsiChar;
    opened_path: PAnsiChar;
    handle:
    record
      case Integer of
        1:
        (
          fd: Integer;
          );
        2:
        (
          fp: pointer;
          );
        {$IFDEF PHP5}
        3 :
        (
         stream : TZendStream;
         );
        {$ENDIF}
    end;
    free_filename: shortint;
  end;
  TZendFileHandle = zend_file_handle;

  arg_separators =
    record
      output : PAnsiChar;
      input : PAnsiChar;
    end;

  Pphp_Core_Globals = ^TPHP_core_globals;
    Tphp_core_globals =
    record
      magic_quotes_gpc     : zend_bool;
      magic_quotes_runtime : zend_bool;
      magic_quotes_sybase  : zend_bool;
      safe_mode            : zend_bool;
      allow_call_time_pass_reference : boolean;
      implicit_flush : boolean;
      output_buffering : Integer;
      safe_mode_include_dir : PAnsiChar;
      safe_mode_gid : boolean;
      sql_safe_mode : boolean;
      enable_dl :boolean;
      output_handler : PAnsiChar;
      unserialize_callback_func : PAnsiChar;
      safe_mode_exec_dir : PAnsiChar;
      memory_limit : Longint;
      max_input_time : Longint;
      track_errors : boolean;
      display_errors : boolean;
      display_startup_errors : boolean;
      log_errors : boolean;
      log_errors_max_len : Longint;
      ignore_repeated_errors : boolean;
      ignore_repeated_source : boolean;
      report_memleaks : boolean;
      error_log : PAnsiChar;
      doc_root : PAnsiChar;
      user_dir : PAnsiChar;
      include_path : PAnsiChar;
      open_basedir : PAnsiChar;
      extension_dir : PAnsiChar;
      upload_tmp_dir : PAnsiChar;
      upload_max_filesize : Longint;
      error_append_string : PAnsiChar;
      error_prepend_string : PAnsiChar;
      auto_prepend_file : PAnsiChar;
      auto_append_file : PAnsiChar;
      arg_separator : arg_separators;
      gpc_order : PAnsiChar;
      variables_order : PAnsiChar;
      rfc1867_protected_variables : THashTable;
      connection_status : Smallint;
      ignore_user_abort : Smallint;
      header_is_being_sent : Byte;
      tick_functions : zend_llist;
      http_globals : array[0..5] of pzval;
      expose_php : boolean;
      register_globals : boolean;
      register_argc_argv : boolean;
      y2k_compliance : boolean;
      docref_root : PAnsiChar;
      docref_ext : PAnsiChar;
      html_errors : boolean;
      xmlrpc_errors : boolean;
      xmlrpc_error_number : Longint;
      modules_activated : boolean;
      file_uploads : boolean;
      during_request_startup : boolean;
      allow_url_fopen : boolean;
      always_populate_raw_post_data : boolean;
      {$IFDEF PHP510}
      report_zend_debug : boolean;
      last_error_message : PAnsiChar;
      last_error_file : PAnsiChar;
      last_error_lineno : integer;
      {$IFNDEF PHP530}
      error_handling : error_handling_t;
      exception_class : Pointer;
      {$ENDIF}
      disable_functions : PAnsiChar;
      disable_classes : PAnsiChar;
      {$ENDIF}
      {$IFDEF PHP520}
      allow_url_include : zend_bool;
      com_initialized : zend_bool;
      max_input_nesting_level : longint;
      in_user_include : zend_bool;
      {$ENDIF}

      {$IFDEF PHP530}
      user_ini_filename : PAnsiChar;
      user_ini_cache_ttl : longint;
      request_order : PAnsiChar;
      mail_x_header : zend_bool;
      mail_log : PAnsiChar;
      {$ENDIF}
    end;

var
  zend_error: procedure(ErrType: integer; ErrText: PAnsiChar); cdecl;
  ts_resource_ex : function(id: integer; p: pointer): pointer; cdecl;

  tsrm_startup: function(expected_threads: integer; expected_resources: integer; debug_level: integer; debug_filename: PAnsiChar): integer; cdecl;
  tsrm_shutdown: procedure(); cdecl;

  sapi_startup: procedure (module : pointer); cdecl;
  sapi_shutdown:  procedure; cdecl;

  php_module_startup: function(sf : pointer; additional_modules : pointer; num_additional_modules : uint) : Integer; cdecl;
  php_register_variable: procedure(_var : PAnsiChar; val: PAnsiChar; track_vars_array: pointer; TSRMLS_DC : pointer); cdecl;
  php_execute_script : function (primary_file: pointer; TSRMLS_D : pointer) : integer; cdecl;
  php_request_startup: function(TSRMLS_D : pointer) : Integer; cdecl;
  php_request_shutdown: procedure(dummy : Pointer); cdecl;
  php_module_shutdown_wrapper:  function (globals : pointer) : Integer; cdecl;

function GetSAPIGlobals(TSRMLS_DC : pointer) : Psapi_globals_struct;
function PG(TSRMLS_DC : pointer) : Pphp_Core_Globals;

function PHPLoaded : boolean;
procedure LoadPHP(const DllFolder: String);
procedure UnloadPHP;

implementation

var
  PHPLib : THandle = 0;
  MyPHP : boolean = False;

function PHPLoaded : boolean;
  begin
  Result := PHPLib <> 0;
  end;

function GetSAPIGlobals(TSRMLS_DC : pointer) : Psapi_globals_struct;
  var
    sapi_global_id : pointer;
    sapi_globals_value : integer;
    sapi_globals : Psapi_globals_struct;
  begin
  Result := nil;
  sapi_global_id := GetProcAddress(PHPLib, 'sapi_globals_id');
  if Assigned(sapi_global_id) then
    begin
    sapi_globals_value := integer(sapi_global_id^);
    asm
      push ecx
      push edx
      push eax

      mov ecx, sapi_globals_value
      mov edx, dword ptr tsrmls_dc
      mov eax, dword ptr [edx]
      mov ecx, dword ptr [eax+ecx*4-4]
      mov sapi_globals, ecx

      pop eax
      pop edx
      pop ecx
      end;
    Result := sapi_globals;
    end;
  end;

function GetPHPGlobals(TSRMLS_DC : pointer) : Pphp_Core_Globals;
  var
    core_global_id : pointer;
    core_globals_value : integer;
    core_globals : Pphp_core_globals;
  begin
  Result := nil;
  core_global_id := GetProcAddress(PHPLib, 'core_globals_id');
  if Assigned(core_global_id) then
    begin
    core_globals_value := integer(core_global_id^);
    asm
      push ecx
      push edx
      push eax

      mov ecx, core_globals_value
      mov edx, dword ptr tsrmls_dc
      mov eax, dword ptr [edx]
      mov ecx, dword ptr [eax+ecx*4-4]
      mov core_globals, ecx

      pop eax
      pop edx
      pop ecx
      end;
    Result := core_globals;
    end;
  end;

function PG(TSRMLS_DC : pointer) : Pphp_Core_Globals;
  begin
  result := GetPHPGlobals(TSRMLS_DC);
  end;

procedure LoadPHP(const DllFolder : String);
  begin
  if PHPLoaded then Exit;

  MyPHP:=True;

  {$IFDEF PHP5}
  PHPLib := GetModuleHandle(PChar(DllFolder+'php5ts.dll'));
  if PHPLib=0 then
    PHPLib := LoadLibrary(PChar(DllFolder+'php5ts.dll'))
  else
    MyPHP:=False;
  {$ELSE}
  PHPLib := GetModuleHandle(PChar(DllFolder+'php4ts.dll'));
  if PHPLib=0 then
    PHPLib := LoadLibrary(PChar(DllFolder+'php4ts.dll'))
  else
    MyPHP:=False;
  {$ENDIF}

  if PHPLib = 0 then
    raise Exception.CreateFmt('PHP DLL not found in Folder %s', [DllFolder]);

  zend_error := GetProcAddress(PHPLib, 'zend_error');
  ts_resource_ex := GetProcAddress(PHPLib, 'ts_resource_ex');

  tsrm_startup := GetProcAddress(PHPLib, 'tsrm_startup');
  tsrm_shutdown := GetProcAddress(PHPLib, 'tsrm_shutdown');

  sapi_startup := GetProcAddress(PHPLib, 'sapi_startup');
  sapi_shutdown := GetProcAddress(PHPLib, 'sapi_shutdown');

  php_request_startup := GetProcAddress(PHPLib, 'php_request_startup');
  php_request_shutdown := GetProcAddress(PHPLib, 'php_request_shutdown');

  php_module_startup := GetProcAddress(PHPLib, 'php_module_startup');
  php_module_shutdown_wrapper := GetProcAddress(PHPLib, 'php_module_shutdown_wrapper');

  php_register_variable := GetProcAddress(PHPLib, 'php_register_variable');
  php_execute_script := GetProcAddress(PHPLib, 'php_execute_script');
  end;

procedure UnloadPHP;
  var
    H : THandle;
  begin
  if PHPLib<>0 then
    begin
    H := InterlockedExchange(integer(PHPLib), 0);
    if MyPHP and (H > 0) then
      begin
      MyPHP:=False;
      PhpLib:=0;
      FreeLibrary(H);
      end;
    end;
  end;

initialization
finalization
UnloadPHP;
end.
