# R package for time-course analysis 

To install this package on your local machine:

1. First, you need to install the [devtools](https://github.com/hadley/devtools) package. You can do this from CRAN. Start R and then type:

```
install.packages("devtools")
```

2. Load the devtools package:

```
library(devtools)
```

3. In most cases, you just use `install_github("author/package")`. For example, with this *tca* package, which exists at [github.com/dmattek/tca-package](github.com/dmattek/tca-package), you’d type:

```
install_github("dmattek/tca-package")
```

**Note**
On OSX, if you encounter following error during installation:

```
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/username/Library/R/3.5/library/rJava/libs/rJava.so':
  dlopen(/Users/username/Library/R/3.5/library/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib
  Referenced from: /Users/username/Library/R/3.5/library/rJava/libs/rJava.so
  Reason: image not found
  ```
  
- Make sure you have Java Development Kit installed from [Oracle website](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
- Open Terminal.app and type (using an account with admin privileges):
  
  ```
  sudo R CMD javareconf
  ```
