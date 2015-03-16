Once you have Mercurial, you can check Jeeves out as follows:
```
hg clone https://code.google.com/p/jeeveslib/
```
Once you have SBT, you can compile Jeeves:
  1. Run `sbt update` to pull in the necessary dependencies.
  1. Run `sbt compile` to compile.
Once you installed, you can run `sbt test` to run the tests.
  1. Run the tests by running `sbt test`.

# Linux #

The Linux installation should be fairly straightforward as long as you are using a version of Z3 3.2 or later and either putting the binary in the /opt/z3 directory or setting your smt.home variable appropriately.


# Windows #
  1. Download Scala 2.10.0.
  1. Download [Cygwin](http://www.cygwin.com/). Make sure wget, chmod, and major defaults are installed.
  1. Add Cygwin to [path](http://thehacklist.blogspot.com/2009/04/cygwin-ls-command-not-found.html).
  1. Download Z3.
    1. Go to http://z3.codeplex.com/
    1. Download the Z3 version 3.2 zip file.
    1. Unzip the internal `z3-xxx-xx` folder and store to a temporary location
  1. Copy/Move the contents of `z3-xxx-xx` from temporary download location and to `Cygwin/.../opt/z3`.
  1. Download [SBT](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html) build tool, version 0.10.0 or above.
  1. Install [Mercurial](http://mercurial.selenic.com/wiki/Download#Windows). Recommended to use the msi that "needs admin rights to install," InnoSetup, or TortoiseHg.
  1. Use Cygwin to navigate to the `jeeveslib`

# Mac #
These instructions are for OSX 10.7+.
  1. Install Wine using MacPorts
    * Prerequisites:
      * Apple [Xcode/XQuartz](http://www.google.com/url?q=https%3A%2F%2Fitunes.apple.com%2Fus%2Fapp%2Fxcode%2Fid497799835%3Fmt%3D12&sa=D&sntz=1&usg=AFrqEzcqyx1EujVZ4hyTqmznfXrVvjFOwg) After installation be sure to install command line tools: go to Xcode -> Preferences -> Downloads.
      * [Java Development Package (Detailed Instructions)](http://www.google.com/url?q=http%3A%2F%2Fhelpx.adobe.com%2Fx-productkb%2Fglobal%2Finstall-java-jre-mac-os.html&sa=D&sntz=1&usg=AFrqEzehJB4oNSidinr5JOHeEktGwvX24A)
  1. Install [MacPorts](http://www.google.com/url?q=http%3A%2F%2Fwww.macports.org%2Finstall.php&sa=D&sntz=1&usg=AFrqEze2J-tU4p47Ab0r7Vv5feZ7S1SUrQ) by downloading the compatible ".dmg" package.
  1. Configure MacPorts:
    1. Open a terminal and copy paste the following code into it:
```
echo export PATH=/opt/local/bin:/opt/local/sbin:\$PATH$'\n'export MANPATH=/opt/local/man:\$MANPATH | sudo tee -a /etc/profile
```
> > > If MacPorts was installed properly then you should see as a response the following code; if not then the Admin Account does not have a password set which is required.
```
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export MANPATH=/opt/local/man:$MANPATH
```
      1. Next, run the following command:
```
if [ `sysctl -n hw.cpu64bit_capable` -eq 1 ] ; then echo "+universal" | sudo tee -a /opt/local/etc/macports/variants.conf; else echo "not 64bit capable"; fi
```
> > > > The terminal should respond with `+universal`.
      1. Next, run the following code to accept the Xcode license:
```
sudo xcodebuild -license
```
      1. Quit and reopen the terminal.
  1. Install Wine using MacPorts:
    1. This part will can take a really long time depending on the speed of your computer. It will take approximately 1-2 hours, so you can walk away and do something else in the meantime; eat, play video game, read, etc. Note: if after running the following command, Terminal complains about not finding 'make' then you probably did not install Xcode/XQuartz properly and should refer to step 1.2.
```
sudo xcodebuild -license
```
    1. Run the following two commands to ensure the D-Bus is running properly:
```
sudo launchctl load -w /Library/LaunchDaemons/org.freedesktop.dbus-system.plist
launchctl load -w /Library/LaunchAgents/org.freedesktop.dbus-session.plist
```
    1. Reference for previous installation Instructions: http://www.davidbaumgold.com/tutorials/wine-mac/#what-is
  1. Installing Z3 SMT Solver for Mac OSX:
    1. Navigate to http://research.microsoft.com/en-us/um/redmond/projects/z3/old/older_z3.html and download the preferred ".msi" file.
    1. Open Terminal, if it is not already, and move the file from Downloads/ to your desired directory and use the msiexec wine command to install z3.
```
wine msiexec name_of_file.msi
```
    1. Create executable:
      1. If you have not already, make a ~/opt/z3/bin/ directory and cd into it.
      1. With your preferred editor, open a new file and name it z3.
      1. Inside, paste the following instructions:
```
args=`echo $* | sed -e 's|/|\\\|g'`
export WINEDEBUG=fixme-all
exec wine ~/.wine/drive_c/bin/z3.exe $args
```
      1. Type `chmod a+x z3` in order to make the file executable by all users, groups and others.
      1. Test that z3 was correctly installed by checking `./z3 -version`.
  1. MacPort Mercurial & sbt:
    1. sbt:
```
sudo port install sbt
```
    1. Mercurial:
```
sudo port install mercurial +bash_completion
```
    1. If these two options do not work properly than you can download Homebrew and install mercurial and sbt with the following commands:
```
brew update
brew install sbt
brew install mercurial
```