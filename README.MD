# RealThinClient SDK

RealThinClient SDK is a flexible and modular framework for building reliable and scalable cross-platform Applications with Delphi, designed for the Web by utilizing HTTP/S with full IPv4 & IPv6 support and built-in multi-threading, extensively stress-tested to ensure the highest stability. By using non-blocking event-driven communication with built-in thread-pooling, Applications built with the RealThinClient SDK can handle thousands of active connections by using a limited number of threads in a real multi-threaded environment, on all supported platforms.



[License](License.txt)

[How to Install]( Installation.md )

[Product web page](https://rtc.teppi.net)


#### Multi-Deployment: target Windows, Mac OSX, iOS and Android from a single code-base

Write one Project in Delphi and compile it as (a) robust stand-alone Client and/or Server for Windows, Mac OSX, iOS or Android, or … as (b) ISAPI DLL extension for MS IIS or Apache, or … (c) integrate your Application into third-party Message-based Servers like the NexusDB Server.

#### RAD: Write events, not interfaces

Everything you do with RTC is component-based, while all the code you will ever need to write is defined as events, so you can design your server and client applications, without the need to use wizards or write interfaces.

#### Easy MultiThreading: turn on/off with a simple switch

A very sophisticated Threading mechanism is built into all RTC Connection components, which allows thousands of active connections to be running in separate threads, all at the same time. And the only thing you need to do is change a simple MultiThreaded property to True. Then, any newly received connection will be handled by a thread pooling mechanism which saves valuable System resources, while still enabling nearly unlimited number of simultaneous connections at the same time.

#### HTTP(S): No Firewall problems

RTC uses HTTP/S for all communication, which makes it possible for RTC Clients to work through most corporate Firewalls and Proxy Servers simply by setting the UseProxy property to TRUE. And if Automatic Proxy Recognition is not configured on the Client PC, or the Proxy requires manual login, you can manually configure RTC components to work through the Proxy, by using data provided by the Network/Proxy Admin.

#### Non-Blocking

Other than most other internet components, RTC connection components are non-blocking and event-driven. This means that, even when not using a multithreaded mode, your users will not be blocked (“Application Not Responding”) during communication between your client and the server. For each state change in the connection, there is an event that will be triggered.

#### Remote Functions-reusable, easy to write, easy to call

Implementing and using remote functions is as easy as writing local functions. You just place a RtcFunction component on your Form or a Data Module, set function name and write your OnExecute event. OnExecute event receives the Connection object, Function Parameters and a Result object. You just have to use the parameters passed to the function to prepare the result. The rest is done “under the hood” by RealThinClient components.

#### Strong encryption & automatic compression

RTC components provide built-in and easy-to-use strong encryption as well as automatic data compression when working with Remote Functions. Enabling or disabling strong encryption and/or compression for remote functions is completely transparent. Simply set Encryption and/or Compression properties on RtcClientModule and RtcServerModule components and leave the rest to RTC. No other chnages in code required.

#### Plug-In Framework-write your own plug-ins or use 3rd-party plug-ins

All Client and Server code you write using RealThinClient SDK is ready to be used as building blocks or plug-ins in any application that uses RealThinClient SDK. For example, you can combine a Web Server, Messenger server, Application server, Database server and your own functions in one Data Server, even when those components weren’t specially designed to share the same Server space. Because of the easy-to-use plug-in framework on top of which all RTC Components are built, you can link unlimited number of functionality and data providing components like RtcDataProvider, RtcServerModule and/or RtcDataServerLink to one Data Server connection component, enhancing that server’s functionality with each new component. And by keeping code in separate units (Data Modules), you can even provide your own plug-ins (and sell those plug-ins) to anyone who uses the RealThinClient SDK.

#### Small footprint

A complete Web Server implementation, using RealThinClient SDK, is less than 700 KBytes when compiled with D7. Even though it is so small, it offers enough functionality with great stability and scale-ability.



# Maximum Flexibility for Remote Functions, Sessions and Scripting

RealThinClient SDK supports all native and complex data types for working with Sessions and Remote Functions. You can use any data type to store and access data in a session and/or to send and receive data between clients and servers using RTC remote functions.

All complex data types (record, array, dataset, function) can hold objects of any data type. For example, you can put integers, strings, records, arrays datasets and even remote function calls inside other arrays, records and datasets, as well as use any combination of RTC data types as parameters and/or return values from RTC remote functions.

| RTC Type   | Access  / Make New           | Description                                                  |
| ---------- | ---------------------------- | ------------------------------------------------------------ |
| any        | isType                       | returns  type of data stored in a specific variable, used to check if and what kind of  data has been received as parameters to a remote function call on the Server  and what kind of data was received as a result of a remote function call on  the Client. |
| any        | CheckType                    | returns  TRUE if data stored in a specific variable is compatible with the type passed  in as a parameter. |
| Null       | isNull                       | returns  True if parameter is not set (it is “NULL”). You can set “isNull” to True to  clear the variable/parameter. |
| Boolean    | asBoolean                    | Boolean  value (Delphi type: boolean)                        |
| Integer    | asInteger                    | Integer  value (Delphi type: longint)                        |
| LargeInt   | asLargeInt                   | Large  Integer value (Delphi type: int64)                    |
| OID        | asOID                        | Linked  Object Identifier (Delphi type: int64)               |
| Cardinal   | asCardinal                   | Cardinal  integer value (Delphi type: cardinal)              |
| Float      | asFloat                      | Float  value (Delphi type: double)                           |
| Currency   | asCurrency                   | Currency  value (Delphi type: currency)                      |
| DateTime   | asDateTime                   | Date  and Time value. Always use this type to work with Dates and Times (Delphi  type: TDateTime) |
| String     | asString                     | String  value (Delphi type: AnsiString, up to 2GB in size).  |
| Text       | asText                       | Text  value, automatically coded and encoded using UTF-8 (Delphi type: String /  UnicodeString). |
| WideString | asWideString                 | Wide  String value (Delphi type: WideString, up to 2GB in size) |
| Variable   | asVariable                   | use  to send/receive variable names (Delphi type: String / UnicodeString) |
| Exception  | asException                  | use  to send/receive exception message (String / UnicodeString). If exception is  raised from the OnExecute event (and not handled by that event), client will  receive the exception message “asException”. |
| ByteStream | asByteStream / newByteStream | use  to work with Streams. When you use “newByteStream”, an in-memory stream will  be created, which you can access using “asByteStream”. When used with remote  functions, data stored in a stream will be sent/received between  client/server as any other data type. |
| ByteArray  | asByteArray / newByteArray   | use  to work with Byte Arrays. When you use “newByteArray”, a byte array will be  created in memory, which you can access using “asByteArray”. When used with  remote functions, data stored in a byte array will be sent/received between  client/server as any other data type. |
| Record     | asRecord / newRecord         | use  to work with records. In records, each element is accessed using a name  (string). Each element in a record can be of any type (even an array, record,  dataset or function call). |
| Array      | asArray / newArray           | use  to work with arrays. In arrays, each element is accessed using an index of  type integer. Each element in an array can be of any type (even an array,  record, dataset or function call). |
| DataSet    | asDataSet / newDataSet       | use  to work with datasets. In datasets, you browse/insert/delete records as you  would usually do with Delphi’s TDataSet (using  prior/next/insert/append/delete), while you have access to fields from a  current record using names (same as in a record). Each field in a dataset can  be of any type (even an array, record, dataset or function call). |
| Function   | Param / newFunction          | use  “newFunction” to prepare a remote function call, setting function name and  all parameters. Each parameter can be of any type supported by TRtcValue.  This means that one parameter of TRtcFunctionInfo can hold virtually  anything, from a simple value to a complex data structure or another remote  function call (even a complete set of function calls). |



## Featured 3rd-party components working with the RealThinClient SDK



**StreamSec** Tools is a high performance cryptographic security library with support for both low level operations,such as RSA and AES, and higher level schemes, such as SSL/TLS and S/MIME. Anything faster is either covered by patents, hardware, or a major scientific break through. StreamSec Tools 4.x can be used to add native SSL/TLS support to RealThinClient components. For more info, visit [StreamSec](http://www.streamsec.com/).



**NexusDB** is an ultra-fast, client/server database engine designed for the Delphi and C++Builder developer. It is nimble enough to be fully embedded into your desktop applications and powerful enough to be your primary database server. RealThinClient components can be used directly over NexusDB transports, combining their features into a single coherent entity. For more info, visit [NexusDB](http://www.nexusdb.com/)
