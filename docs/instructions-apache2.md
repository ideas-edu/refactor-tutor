# Installation Instructions Apache2

If you want to start using apache2 directly, you will first need to create a folder of exercises. These have the following structure:

```
exercises/
  name-of-ex1/
    start.java
    description.txt
    tests.txt
```

Each exercise needs a start situation, a textual description and a set of test cases showing correct behaviour. Folders starting with a dot `.` are ignored. Examples of these can be found in the repository under the exercises folder.
Once you have a set of exercises, you can download the latest release from the [releases page on github](https://github.com/ideas-edu/refactor-tutor/releases/latest).

You will need to set up two things: The backend CGI service and the static html/javascript frontend. To do this you will need to download `rpt-web.tar.gz`, unpack it and put the contents of the `www` folder in a place where apache can find it. Some examples of these places are `/var/www/html` or `/usr/local/apache2/htdocs`. 

Once you have finished with the frontend, you need to upload the backend. To do this, download `rpt-cgi.tar.gz` and extract it to a cgi-enabled folder, for example `/usr/local/apache2/cgi-bin`. To make this folder work, you will usually need to edit apache config. Add or uncomment the following line:

```apache2
ScriptAlias /cgi-bin/ "/usr/local/apache2/cgi-bin/"
```

The frontend expects the cgi backend to live relative to the frontend pages on `cgi-bin/rpt` and `cgi-bin/login`. If you don't have the same folder structure, for example if you host the frontend in a subdirectory of the root, you will need to edit the `BASE_URL` line in the frontend's `rpt.js'.
Finally, to enable or disable the login screen, use one of the following options:

1. Replace the `login` file next to the `rpt` cgi file. To enable the login screen enter the value `true` in the file, to disable it enter `false`. 
2. Leave the `login` file as it is and add the following line to your apache config.

```apache2
SetEnv LOGIN_ENABLED false
```

After all these steps you should have something similar to the following:

```
/usr/local/apache2/
  htdocs/
    img/
    index.html
    login.html
    rpt.js
  cgi-bin/
    exercises/
    rpt
    login
```
