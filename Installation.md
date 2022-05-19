

INSTALL RTC SDK components in Delphi
--------------------------------

After you have unpacked the files in a folder of your choice and started Delphi,
you should add the path to the RTC SDK's "Lib" folder to "Library paths" in Delphi.

In older Delphi versions, the Library Path is located in the "Tools / Environment Options" menu.
Select the "Library" tab and add the full path to the RTC SDK's "Lib" folder to "Library path".

In newer Delphi versions, Library Paths are located in the "Tools / Options" menu. 
Select the "Environment Options / Delphi Options / Library" tree branch, where you will 
find the "Library Path" field. There, you should click the "..." button next to 
the "Library path" and add the path to the RTC SDK's "Lib" folder.

In Delphi XE2 and later, you will also see a "Selected Platform" drop-down menu. 
There, all the settings are separated by platforms, so you  will need to 
repeat the process for every platform you want to use the "RTC SDK" with.

Once you have configured the "Library path" (so the IDE can find RTC SDK files), open the
"SDKPackages_Main" Project Group, containing these 2 runtime and 2 design-time packages:

  rtcSDK.dpk       -> The main runtime Package, contains all Client and Server components. 
  rtcSDKD.dpk      -> The main design-time Package, registers all Client and Server components.

Install the components in Delphi by using the "Install" button, or the "Install" menu option.
In older Delphi versions, you will see the "Install" button in the Project Manager window.
In newer Delphi versions, you will find the "Install" option if you right-click the package
file in the Project Manager accessed in the "View" drop-down menu.

When compiled and installed, you will see a message listing all components installed.

NOTE: You should ONLY compile and install all RTC packages for the Win32 platform, because the 
Delphi IDE is a Win32 Application. First compile and install runtime packages, then desig-time.

NOTE: When switching Projects or changing the active target platform on a Project, 
always use BUILD (not COMPILE) to "compile" your Project(s), because RTC uses 
compiler directives to build the same source code for different platforms.



UPDATE RTC SDK components in Delphi
-------------------------------

To update RTC SDK components, before installing new RTC packages, it is 
adviseable to uninstall old RTC packages and delete the old BPL and DCP files:

  - rtcSDK.bpl & rtcSDK.dcp
  - rtcSDKD.bpl & rtcSDKD.dcp

To uninstall RTC SDK components, after you start Delphi, 
open the menu "Component / Install Packages ..." where you 
will see a list of all packages currently installed in your Delphi. 

Scroll down to find "RealThinClient SDK" and click on it (single click). 
When you select it, click the button "Remove" and Delphi will ask you 
if you want to remove this package. Clicking "Yes" will uninstall the RTC SDK.

After that, *close* Delphi and follow step (2) to install the new RTC SDK package.

NOTE: Uninstalling the RTC SDK package will also uninstall all packages which 
are using the RTC SDK (for example "rtcSDK_DBA" and "RTC Portal" packages). 
So ... if you are using "RTC Portal" or any other product using the RTC SDK, you will 
need to Build and Install all related packages again, after you reinstall the RTC SDK.



Make RTC SDK accessible from XCode - for iOS development with Delphi XE2
-------------------------------

For the FPC compiler to find RTC SDK files, you can either copy the complete "Lib" folder (with sub-folders)
from the RTC SDK package into the "/Developer/Embarcadero/fmi" folder (quick and dirty solution), or ... 

You can add the path to the RTC SDK "Lib" folder (located on your Windows PC, made accessible to Mac over LAN) 
to the FPC search path. Unfortunatelly, there  is no parameter for adding FPC search paths in XCode directly, 
so you will need to do this manually for every XCode Project. And not only once, but every time you recreate 
XCode Project files by using the "dpr2xcode" tool, because all your changes will be overwritten by "dpr2xcode". 

To avoid having to make these changes too often, use "dpr2xcode" ONLY if you have made changes to the Project 
file itself (changed the Project icon, for example). There is no need to recreate XCode Project files if you 
have only changed forms or units inside the Project.

To add the RTC SDK paths to FPC, you will need to modify the file "xcode/<ProjectName>.xcodeproj/project.pbxproj". 
The path to the RTC SDK "Lib" folder needs to be added as two new parameters. Once for iOS-Simulator and 
once for iOS-Device compilation, both of are configured through the shellScript parameter.

The best place to add the RTC SDK Lib path is after the FireMonkey path, so you should search for
"/Developer/Embarcadero/fmi" in the above mentioned XCode Project file. You will find 2 such 
instances in the "ShellScript" line and you should add the path to the RTC SDK Lib folder directly 
after each "/Developer/Embarcadero/fmi" instance.

For example, if you have made the complete RTC SDK folder on your Windows PC available to your Mac OSX 
through a network share named "RTC_SDK" (read/write access rights to that folder will be required for FPC 
compilation to work), you should add /Volumes/RTC_SDK/Lib after both /Developer/Embarcaedro/fmi locations. 
One is for the iOS-Simulator, the other one for the iOS device. 

That will be enough to let FPC know where to look for RTC SDK files.

Should you still get "File not found" errors when trying to compile a Project using RTC files, 
make sure the path you have used is correct and that Mac OSX has read and write access to that folder.

PS. Before configuring access to the RTC SDK, you will need to have OSX 10.6 or 10.7, the latest XCode 4.x 
version and both packages provided by Embarcadero (included with RAD Studio XE2) installed on your Mac. 

To make sure your Mac OSX configuration is correct, create an empty "FireMonkey iOS HD" Project, 
use "dpr2xcode" to create XCode Project files and try to run that Project from XCode,
either inside the iOS-Simulator or directly on your iOS device (iPhone or iPad).



Documentation
-------------

The best place to start learning about RTC SDK is the [online tutorials]( https://rtc.teppi.net/classroom/ ) and the Quick Start Guide.

After going through the online lessons, you should also go through the  examples included in the RTC SDK package. 

When you are done examining online lessons' examples, I suggest browsing through the FAQ. Even if you won't
be reading all the articles, you should at least get the feeling about the information included there.

RTC SDK Demos are another good source of information, including a lot of examples and best practices 
for using the RealThinClient SDK. And the most extensive source of information on the RealThinClient SDK 
are Help files. Some of the information is spread across the files, but if you know which class you need, 
you will most likely be able to find what you are looking for.

When you start working on your project, the FAQ will come in handy when you have to do something 
specific (use Sessions, accept form post data, write and call remote functions, etc). The FAQ is 
continually being extended, as more questions come in.

The latest Help file for Off-line viewing is in the "Help" folder:
- "docs\Help\RTCSDK_Help.chm"



## Examples

You can find all RTC SDK Example Projects in the "src\Examples" folder.

Most FireMonkey Projects require a specific Delphi version to compile,
but can be compiled for all target platforms supported by that Delphi version.
Projects using FireMonkey (Delphi XE2 and later) are organized into 4 Project Groups:

  * Examples\SDKDemos_FMX - Demos using FMX (Delphi XE2) with the rtcSDK(D).dpk
  * Examples\SDKDemos_FMX2 - Demos using FMX2 (Delphi XE3 - XE7) with the rtcSDK(D).dpk
  * Examples\SDKDemos_FMX3 - Demos using FMX3 (Delphi XE8 and later) with the rtcSDK(D).dpk

Other Projects can be compiled with ALL supported Delphi versions, but can only target the Windows platform.
Projects for DLLs, Console- and VCL-Applications are compatible with all Delphi versions down to Delphi 7:

  * Examples\SDKALL_VCL_Examples - ALL Projects that compile into a DLL, Console or VCL Application
  
  * Examples\SDKQuickStart_VCL - Short "QuickStart" examples using the VCL with the rtcSDK(D).dpk
  * Examples\SDKDemos_CON - Console and ISAPI DLL Demos using only the rtcSDK(D).dpk
  * Examples\SDKDemos_VCL - Demos using the VCL with the rtcSDK(D).dpk
  * Examples\SDKTools_VCL - A collection of simple RTC Tools with focus on RTC development and testing

Descriptions of most Examples can be found in the "Examples\Readme_Examples.txt" file.