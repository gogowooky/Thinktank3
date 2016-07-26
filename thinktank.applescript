tell application "Terminal"
	do script "echo dummy"
	do script "cd /Users/gogowooky/Dropbox/MyJobs/Thinktank3/" in window 1
	do script "ruby thinktank.rb --memodir /Users/gogowooky/Dropbox/MyData/tt/" in window 1
end tell
