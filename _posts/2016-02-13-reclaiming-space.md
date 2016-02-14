---
layout: post
title:  "Duct Tape DevOps Part 1: Reclaiming Hard Drive Space"
date:   2016-02-13 22:53:02
categories: devops
---

At ROI Revolution we work on internal Rails applications that we host in-house. As a small engineering team we do not have anyone dedicated to DevOps. As a result, server work falls to the development team. Until recently a lot of this work has been what I would call "Duct Tape DevOps". As our suite of tools continues to grow we are realizing how unsustainable this approach is and are making strides to ["treat our servers like cattle and not pets"](https://blog.engineyard.com/2014/pets-vs-cattle) (I still don't want to give up our X-Men themed naming structure though). Unfortunately, we haven't been able to put away our duct tape yet. While I hope future posts can cover best practices and positive changes we have made in the meantime I want to share some tips and tricks we have used in the meantime.

Now that some of our servers have been around a few years the clutter is coming to a head and we are beginning to run into issues of full hard drives. While competent with the Unix command line, commands like `find` and `du` are not in the forefront of my toolbelt. I hope that this post serves as a handy reference for myself and others on how to leverage these tools to take command and reclaim hard drive space.

# How much space do you have?

Before you begin trying to free up hard drive space it would be good to know how much space you have and how much you are using. For this you would use the `df` utility.

From the man pages:

> The df utility displays statistics about the amount of free disk space on the specified filesystem or on the filesystem of which file is a part.

Running `df` results in output like

```
$ df
Filesystem                        512-blocks      Used Available Capacity  iused    ifree %iused  Mounted on
/dev/disk1                         974716928 651548432 322656496    67% 81507552 40332062   67%   /
devfs                                    657       657         0   100%     1137        0  100%   /dev
map -hosts                                 0         0         0   100%        0        0  100%   /net
map auto_home                              0         0         0   100%        0        0  100%   /home
map -fstab                                 0         0         0   100%        0        0  100%   /Network/Servers
```

and here is the output from a machine that has multiple physical drives

```
$ df
Filesystem    512-blocks      Used Available Capacity   iused    ifree %iused  Mounted on
/dev/disk0s2   975093952 825208992 149372960    85% 103215122 18671620   85%   /
devfs                371       371         0   100%       642        0  100%   /dev
/dev/disk1s2   976101344 539109648 436991696    56%  67388704 54623962   55%   /Volumes/Macintosh HD2
map -hosts             0         0         0   100%         0        0  100%   /net
map auto_home          0         0         0   100%         0        0  100%   /home
```

the only change I tend to make when running `df` is to use the `-H` flag to make the display human readable. Here is the output of `df` with the `-H` flag

```
$ df -H
Filesystem                          Size   Used  Avail Capacity  iused    ifree %iused  Mounted on
/dev/disk1                          499G   334G   165G    67% 81509520 40330094   67%   /
devfs                               337k   337k     0B   100%     1139        0  100%   /dev
map -hosts                            0B     0B     0B   100%        0        0  100%   /net
map auto_home                         0B     0B     0B   100%        0        0  100%   /home
map -fstab                            0B     0B     0B   100%        0        0  100%   /Network/Servers
```

now instead of having the count of 512-blocks (or 1k blocks, depending on your system) disk usage is display using `B`, `K`, `M`, `G` for byte, kilobyte, etc.

I used to use the `-h` flag which will give you similar results but the `-h` flag uses base 2 and the `-H` flag will use base 10. For the purposes of clearing up disk space this doesn't really matter but you may be a bit confused why your 500 GB hard drive says it is only 465 GB.

```
$ df -h
Filesystem                          Size   Used  Avail Capacity  iused    ifree %iused  Mounted on
/dev/disk1                         465Gi  311Gi  154Gi    67% 81509551 40330063   67%   /
devfs                              329Ki  329Ki    0Bi   100%     1137        0  100%   /dev
map -hosts                           0Bi    0Bi    0Bi   100%        0        0  100%   /net
map auto_home                        0Bi    0Bi    0Bi   100%        0        0  100%   /home
map -fstab                           0Bi    0Bi    0Bi   100%        0        0  100%   /Network/Servers
```

I tend to focus on the `Avail` column as I work on removing the cruft and the `Capacity` column as checkpoints since percent of disk space used is more server and application dependent.

# Where is everything?

The next command I use while trying to find which directories or files are using the most space. For this I use the `du` or disk usage statistics command.

From the man pages:

> The du utility displays the file system block usage for each file argument and for each directory in the file hierarchy rooted in each directory argument.  If no file is specified, the block usage of the hierarchy rooted in the current directory is displayed.

With the `du` utility I use a few different flag combinations depending on the situation.

## Largest directory

```
du -hd 1
```

This line will tell you total disk usage by each directory in your current directory. The ordering of arguments matter a bit here, the most important part is that `d` comes last in your argument list. The `-d` flag tell `du` the depth you want look down, passing in `1` tell it to stick to the current directory. Similar to `df` above the `-h` flag just makes the output human readable which makes it a bit easier to reason about.

This is generally useful when trying to get your bearings of the server and don't there are no immediate disk hog suspects in mind. This is particularly useful on our staging server which houses multiple applications. Using this in our applications directory helps us quickly see which application is using the most disk space.

```
$ cd ~/WebApps
$ du -hd 1
 81G    ./app1_beta
 75G    ./app2_beta
 30G    ./app3_beta
186G    .

# You can also tell it which directory to look in

$ du -hd1 ~/WebApps
 81G    ./app1_beta
 75G    ./app2_beta
 30G    ./app3_beta
186G    .
```

If you are in a large directory it could be useful to have everything sorted for you. To do this we can pipe the output of `du` to the `sort` utility. We will need to drop the `-h` flag since `sort` will be strictly looking at the numbers and as a result consider something like `250K` larger than `1G`.I tend to also combine this with `less` so I can easily page through the result.

Here is some of the output from my laptop's home directory:

```
$ du -d 1 | sort -rn | less
512505864       .
202243872       ./Parallels
51942824        ./RailsApps
48638568        ./Library
43650848        ./Documents
42791488        ./.docker  # proof I am trying to be better at DevOps
38589312        ./Work
33957344        ./Music
16159064        ./Downloads
11970784        ./.rvm
6749928 ./Pictures
5037584 ./Desktop
5017472 ./.lolcommits
2819832 ./Dropbox
1469872 ./.Trash
```

The `-rn` flags tell `sort` to sort by numbers `-n` and reverse the list `-r` to have the largest first. If you do not want to use `less` I would recommend leaving off the `-r` so the largest files will be the bottom.

My approach with this technique is to follow the trail until I am either in a directory with only files or a large number of directories with sub-directories and files. When I get to the point look one level deep can be less useful.

## I found the directory...now what do I do?

The answer to this question of course is...it varies. It depends on factors such as, what's in the directory, if you have somewhere else to put it, and how desperate for space you are.

### Zip and Ship

For larger files or groups of old files like logs you could look into to the "zip and ship" method. Where you zip (compress the file(s) with something like `gzip`) and ship them off to another server for safe keeping.

If you aren't careful this could result in the other server running into space issues instead. Depending on the files you could look into writing a script to remove these files when the get old enough. This has been our approach up to this point but as we add applications and additional types of files to back up this gets more and more complicated. Now that we are growing up to use Linux VMs we are exploring more grown up solutions like [`logrotate`](http://www.linuxcommand.org/man_pages/logrotate8.html) as well.

### Burn it all

Sometimes it just makes sense to delete the files, especially if they are too old to be useful (*who needs a Resque log from two months ago?*).

Once I am in a directory and trying to find which files are too old or too large to keep around I turn to `find`. With `find` you can search for files that haven't been modified for some number of days

```
# fils in current directory modified 30+ days ago
find . -mtime +30
# fils in current directory modified 60+ days ago
find . -mtime +60
```

or are larger than a certain size.

```
# files larger than 10 MB
find . -size +10M
# files larger than 1 GB
find . -size +1G
```

you can then use the `-exec` flag to execute arbitrary commands on these files ([represented as `{}`](http://stackoverflow.com/a/10363998)). When working with `exec` it may be best to use tell `find` to only look at files with the `-type file`.

```
# delete the files
find . -type file -mtime +60 -exec rm {} \;

# individually gzip each file
find . -type file -mtime +60 -exec gzip {} \;
```

you could also use this to `mv` everything into one directory (in the case you are in a directory with multiple subdirectories) to more easily examine them all or `scp` them to another server.

# Conclusion

While prevention is the best way to combat dwindling resources, sometimes you have to get your hands dirty and clean out some of the muck. Hopefully these examples of `df`, `du`, and `find` can help make this dirty job a bit easier and allow for some confidence when poking around your servers' file systems.

Do you have any tips or tricks on how you combat full disks? What are your best practices for avoiding the situation altogether? Please comment below and share your DevOps secrets!
