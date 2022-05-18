If you have multiple Delphi versions (up to 4) running on the same PC,
to avoid problems with Delphi IDEs using wrong RTC package files, you can 
use files from the root folder to install RTC packages into the 1st IDE,
use files from the "IDE2" folder to install RTC packages into the 2nd IDE,
use files from the "IDE3" folder to install RTC packages into the 3rd IDE
and files from the "IDE4" folder to install RTC packages into the 4th IDE.

Before you can compile any of these packages, you need to add the 
RTC SDK "Lib" folder to the "Library Path" of each Delphi version,
or you will get compile errors when trying to install the packages.