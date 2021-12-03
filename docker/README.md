A dockerfile for simulating Travis.ci locally.
Builds "travis compile" Ruby GEM, and uses this to create test_branch.sh,
which simulates the test run on Travis.

Typical usage:
Build the docker container: docker build . -t build_name:latest
Run the docker container: docker run -it build_name:latest bash
Run the test script: cd /home/travis/build; bash test_branch.sh

To change branches, change lines 4 and 23 in Dockerfile
Add --branch branch_name to the line 4 as a part of the git clone command
Change "develop" to "branch_name" on line 23
