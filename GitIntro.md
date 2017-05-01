# Git and Github:
Git is a distributed version control system (DVCS), where each developer has their own copy of the project and periodically pulls in other users changes and pushes their own.  Github is a website that hosts git repositories and offers ways to interact with the repo.

There are four core elements of Git version control to understand
- The Working Directory
- The Staged Snapshot
- Committed Snapshots
- Development branches

## Basic Git via the command line:
1. Download git: https://git-scm.com/ and run the installer with the default components selected and just step through the installation.  This means you don't need to change any of the options or selections.
2. Full reference available: https://git-scm.com/book/en/v2
3. This mini tutorial is developed from http://rypress.com/tutorials/git/

### Configuring your environment
Open the git bash and type:

```
git config --global user.name "Your Name"
git config --global user.email your.email@example.com
```

### Checking out an exisiting repository
To get started with an existing repository, navigate within the git bash to the folder you want to check out to, e.g:

``` 
cd /c/projects/
```

On GitHub, navigate to the main page of the repository, and under the repository name, click **Clone or download**.  In the Clone with HTTPs section, click to copy the clone URL for the repo.  Then execute the clone command within Git Bash by pasting the URL you cpied after the git clone command, i.e:

```
git clone https://github.com/nickdiorio/my-git-repo.git
```

### Pulling in remote changes
Before you start working on your files for the day, you will want to pull down changes that other people have committed.  You don't have to, but you will be required to before being allowed to push your changes to the remote repo:

``` 
git pull
```

### Making modifications
As you add and modify files within your project, you can see the status of your repo at any time:

``` 
git status
```

This will tell you which files have been modified, and which files are new and not being tracked.  If you want to remove a file from version control, you type:

```
git rm file_name
```

### Adding files to the staging area
For new files you create, you need to tell Git that you want to explicitly add them to version control.  The staging area is basically a place where you stage files before committing them.  It guarantees that you commit only exactly the files that you want in a particular commit.

```
git add orange.html
```

### Committing files
After you've added all the files you want to add to a particular commit, you commit them with a helpful message about what the commit contains.  Commits should contain logical partitions of work that make is easy to look at the log and understand what each commit was for.

```
git commit -m "Adding a webpage about the color orange"
```

### Pushing the commit to the remote repo
So, you've staged files, and committed them.  But now, you've finally got to push them out to remote repo so that everyone else working on the repo knows about your work and can ingest it into their own working copy.  To do this requires write access on the repo you are trying to push to, and you may be prompted for your username and password for github.

``` 
git push
```

### Viewing commit history
To view a history of the repository, you can type any of the following.  The second command condenses the log messages to one line, which is helpful for viewing a quick summary.  You can also do it for a specific file, as the third line demonstrates.

``` 
git log
git log --oneline
git log --oneline orange.html
```
This log will show the author of each commit, the date, and commit message, plus a unique hashid that identifies the commit.

### Reverting a commit
Git is designed to never loses version history, so to revert a change, git will modify the files in question back to their previous state, and then recommits them as a new commit.  The process to do so is fairly simple.  Use git log to obtain the id of the commit you want to undo, and then revert it:

```
git log --oneline
git revert 3e14adc
git push
```

### Undo uncommitted changes
If you've been developing, but haven't committed and want to undo your changes, the process is a little different since there is no commit id to use.  Note, you should be careful using these commands as they operate on the working directory, not on committed snapshots.  They permanently undo changes, so make sure that's what you want.  Otherwise, you could simply commit what you have and then do a git revert to preserve the history.

```
git reset --hard
```
This changes all *tracked* changes to match the most recent commit. 

```
git clean -f
```
This removes all *untracked* files.  

## Branching workflow with Git via the command line
A branch is an independent line of development.  Branches lend themselves to several standardized workflows for collaborative development, which will be emphasized for SAM.  To list existing branches for your project:

```
git branch
```

This will display branches for the project.  If there is only one branch, it will be called `master`.  This is Git's default branch.  The branch with an asterik next to it is the one currently checked out (only one can be checked out at a time).  

## Create a new branch
To create a new branch to work from, and then switch to work from it, simply type:

```
git branch new_branch_name
git checkout new_branch_name
``` 

Now, any changes you make and commit will be isolated from those in the `master` branch.

## Merging changes
Once you've made isolated changes in a branch and tested, and are confident the changes belong in the master branch, you can merge. The merge command always merges into the currently checked out branch.

```
git checkout master
git merge new_branch_name
```

Which merges `new_branch_name` into `master`

## Deleting a branch
To delete a branch that is no longer needed is straightforward.  Git will warn you if you are deleting an unmerged branch.

``` 
git branch -d new_branch_name
```

## Getting started with Github desktop interface
1. Download Github for Windows: https://desktop.github.com/
2. Login with your github info
3. Clone exisiting repositories from github.com, or add previously downloaded repos into the client.
4. View changes in files, choose which files to commit, and commit.
5. Sync with the remote db, which pulls and pushes in one step.

