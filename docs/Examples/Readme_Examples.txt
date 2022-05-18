---------------
BrowserUpload
---------------

Example using RTC SDK components to accept file uploads from a Web Browser.
This demo also demonstrates accessing Form Post variables.

---------------
ClientUpload
---------------

Example sending a file from a RTC Client to a RTC Server (file upload).

---------------
DualServer
---------------

Example connecting the same Data Provider components to multiple Server 
connection components by using the TRtcDualDataServerLink component.

----------
rtcParse1, 2 & 3
----------

Examples using the simple RTC Parser (TRtcParse) for generating dynamic content.
TRtcParse component was created by Cord Schneider and donated to the RTC SDK.

---------------
RemoteFunctions
---------------

Example using RTC SDK components to write a RTC server to provide remote functions
and also write a RTC client to call those remote functions from the server.

---------------
XMLRPCTest
---------------

Small test application using the RTC SDK to check XML-RPC strings by parsing the 
content entered in a memo field to create a TRtcValue object, then using this object
to generate a new XML-RPC string (100% XML-RPC standard compliant). 
Additionaly, this example shows the same data in the RTC format (for comparison).

---------------
WebSockServer
---------------

Server example using Web Sockets to communicate with HTTP Clients and Web Browsers.

---------------
WebSockClient
---------------

Client example using Web Sockets to communicate with HTTP Servers.

---------------
ServerLesson 1 - 4
---------------

Ready-to-run Server Lessons from the Quick-Start guide:
http://www.realthinclient.com/quickstart.htm

Starting with a simple "TIME" Server, moving step-by-step to a WebServer, 
capable of serving local files and folders of any size.

-----------------------------------
"App_Client" & "App_Server" folders
-----------------------------------

App Client, Server and ISAPI demos can be used to stress-test RTC component 
using Remote Functions with strong encryption by opening hundreds of connections 
from each client and flooding the Server/ISAPI with requests.

App Client Demo is ideal for stress-testing RTC remote functions using multiple 
connections in multi-threaded mode, visualy showing activity and stage for each 
connection in a live graph. Client can choose between "Proxy" and standard connection 
components, to see the difference in bandwidth usage and distribution.

To do the test, you can start the App Server as a stand-alone executable or 
install the App ISAPI extension on a ISAPI-capable Web Server. Either will do. 
When running the test using the App ISAPI, you need to add the path to your ISAPI 
extension to "Module File Name" in your client. 

When running the test using App Server as stand-alone executable, you don't need 
to modify the "ModuleFileName", simply enter the correct Server address and port. 

* App_Client/AppClient.dpr

  This is the Application Client demo (exe).

* App_Server/AppServer.dpr

  This is the App Server ready to be compiled as a stand-alone server (exe).

* App_Server/AppISAPI.dpr

  This is the App Server ready to be compiled as ISAPI Extension (dll)

--------------------
"File_Client" folder
--------------------

File Client demo can be used to test response times from different HTTP Servers 
when a number of requests is being sent ("100 x Post" button). You can point the 
File_Client at any Web Server and request any file (or site) to see how fast 
server will respond and whether connection will get dropped before the file arrives. 
For downloads longer then 1 second, average download speed will be calculated 
(displayed right from the progress bar). 

Downloaded content can also be stored to local file(s).

* File_Client.dpr

  Using rtcDataRequest components to send and receive data using the 
  standard HTTP protocol, so it can be used to test any HTTP Server.

--------------------
"File_Server" folder
--------------------

Simple RTC File Server and ISAPI extension, using the rtcFileProvider to 
provide access to server's local files.

* FileISAPI.dpr

  Example creating a Server project to compile DataProviders as ISAPI Extension

* FileServer.dpr

  Example creating a Server project to compile DataProviders as Stand-alone server.

----------------------
"RTC_Messenger" folder
----------------------

RTC Messenger demonstrates the use of remote function with the RTC SDK to
implement a simple instant messaging client and server. 

RTC Messenger Client can be used instantly to connect to any RTC Messenger Server 
(also RTC Web Server or any Web Server using RTC Messenger ISAPI extension) and 
chat with friends and customers. 

Each user's friends and ignore list is kept on the server. 
You see when your friends are online/offline and you can send messages with smilies 
and links just like you are used to with any other Messenger (like MSN or Yahoo). 

* How to use RTC Messenger Client/Server?

1.) Start RTC Messenger Server and click the "Start" button, 
or ... start the RTC WebServer, check the "Include RTC Messenger Server" checkbox 
and click the "Listen" button, or ... install the RTC Messenger ISAPI on your Web Server. 
Either one will make the Messenger Server ready. 

2.) Start RTC Messenger Client and register by entering your Server's address and port 
(default=80), choosing a username and password (anything you like) and clicking the 
"Register" button. If the username you chose is not already taken on that Server, 
you will be logged in automaticaly. You have to remember your username and password, 
because you will need those every time you want to connect to that Messenger Server.

3.) Add friends by clicking the "Add Friend" button and entering your friend's username. 
After your friend goes OnLine and accepts your invitation, you will be able to see 
his/her online/offline status and send him/her messages (online and offline).

4.) To start chatting with a friend, double-click his/her name in your Friends list. 
A new window will open and you can start typing, just like you are used to with other 
Instant Messaging software (like MSN or Yahoo!). 

5.) To log out, click the "Log out" button and to log in again, enter server address 
and port (if not correct), enter your username and password (the one you chose on registration) 
and click the "Login" button. 

* MSG_Client.dpr

  RTC Messenger Client application, using RTC remote functions to communicate with
  the RTC Messenger Server, RTC Messenger ISAPI or RTC WebServer with "Messenger" activated.

* MSG_Server.dpr

  RTC Messenger Server as a simple Stand-alone Server (exe)

* MSG_ISAPI.dpr

  RTC Messenger Server as an ISAPI Extension (dll)

----------------------
"RTC_WebServer" folder
----------------------

RTC Web Server is a fully functional Web Server, built using only RTC SDK. 
It demonstrates the use of rtcDataProvider components to compile a Web Server
by using RTC SDK components. 

RTC Web Server can be set up in less than 5 minutes to serve virtual hosts with 
local files and folders, PHP scripts, ISAPI extensions and streaming video.
It also has a built-in Messenger Server.

* RtcWebServer.dpr

  This is a standard Server and Service in one. It shows you how to compile 
  one executable which can run as a standard application using Windows, or
  be installed as a Service under NT/2000/XP to start automaticaly with Windows.

------------------------------
"RTC_WebPackageManager" folder
------------------------------

RTC Web Package Manager is an example Web Application, where users can log in 
and download the latest content to which they have been granted access for.

Visual design was done using templates (HTML files with place-holders),
so it can be modified without changing application code. 

All planning, coding and design was done in only 2 weeks, by a single Delphi 
developer who had NO prior knowledge of RealThinclient SDK components.

Main requirement for this project was to use only RTC SDK components and things 
that come preinstalled with Delphi. NO other third-party components were used.

In this simple application, Administrator can:
a) add/remove packages. A package will hold one or more files.
b) add/remove/update files per package (upload from browser)
c) add new users (specify Name, username and password)
d) for each user, grant/disallow access to each package separately 
   (with files for download), with a separate expiration date per package.
e) remove users

Normal Users can:
a) log in with their username and password
b) change their password when logged in
c) see packages they have access rights for, and directly download the files
d) see expired packages, with a link for extending the license
e) see a list of non-licensed packages with short description and links for buying a license

To change the server configuration, 
modify the "RtcWebPackManager.ini" file.

To change the Admin username and/or password, 
modify the "users.data" file (plain text).

To test the application, compile and run the "RTCWebPackManager.exe" project, 
then start a Browser and go to> http://localhost


---------------------
"RTC_WebForum" folder
---------------------

RTC Web Forum is an example Web Forum Application, which can be used as it is,
or as a template for designing a Forum which will run under any RTC Server.

There are 4 projects in this folder. 

* Deploy\RTCWebForum.dpr - a stand-alone HTTP server

* Deploy\WebForum_ISAPI.dpr - an ISAPI extension dll

* Deploy\RTCWebForumTLS.dpr - a stand-alone HTTPS server, using a StreamSec plugin for SSL

* Deploy\RTCWebServer2.dpr = RTCWebServer.dpr extended with a Forum

To change the server configuration, modify the "RtcWebForum.ini" 
(or "WebForum_ISAPI.ini" for the ISAPI extension) file.

Default Admin username and password are "admin" (all lowercase).

After logging into the admin area, create a new user with username "admin" and set a new admin password. 
When a user with username "admin" is created, it will be used for administrative login. 
If you should forget the Admin password, you can open the "users.data" file (plain text) 
in the "RtcForumData" folder and look for the user "admin"

To test the stand-alone server, compile and run the "RTCWebForum.exe" project, 
then start a Browser and go to> http://localhost

To test the ISAP extension, compile the "WebForum_ISAPI.dll" project, 
then copy the "WebForum_ISAPI.dll" file, "messages.ini", "WebForum_ISAPI.ini" and the 
complete "RtcForumData" folder to your Web Server's ISAPI folder and install the ISAPI extension.

If you need a complete Web Server with a Forum,
please take a look at the RTC_WebServer project.

----------------------
"DataProviders" folder
----------------------

You will find data modules implementing all DataProvider components used in
RTC SDK Demos, which you can also use from your RTC Server applications.

* rtcFileProvider.pas

  This is a standard File provider, which you can use to add standard file access
  to any RTC Server. This DataProvider is also used by the RTC WebServer Demo.

* rtcISAPIProvider.pas

  This is a standard ISAPI Extension provider, which you can use to add basic ISAPI
  Extension support to any RTC Server (also used by the RTC WebServer).

* rtcMessengerProvider.pas + rtcMessenger.pas

  This is a DataProvider which you can use to add basic Messenger Server capabilities 
  to any RTC Server (used by RTC WebServer, RTC Messenger Server and RTC Messenger ISAPI).
