tell application "Terminal"
do script "echo dummy"
do script "cd /Users/gogowooky/Dropbox/MyJobs/Thinktank3/el/" in window 1
do script "ruby thinktank.rb --test run" in window 1
end tell
