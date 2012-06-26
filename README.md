NOTE
====

This is project abandonware.  The bulk of this code got merged folded into
Snap's default project template.  So use "snap init" to get the code and work
from there.

...

snap-login-form-example
=======================

Simple login form example with Snap's Auth snaplet.

This sample is meant to be minimally dependent on other libraries in
order to better demonstrate basic Snap features.

Instructions
------------

Build as usual with cabal (I've tested with cabal-dev).

Run it.

Navigate to http://localhost:8080

You're prompted with a login form.  By default your app doesn't have
any registered users.  Click on the "Create a new user" link to create
a new user.  Use the form to create a new user.  After user creation,
login with the login information you used for creating the new user.
Upon successful login, you're taken to the index page of your site.
