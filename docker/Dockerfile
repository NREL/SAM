FROM travisci/ci-sardonyx:packer-1576238197-60d50014

# Clone SAM
RUN mkdir /home/travis/build/; cd /home/travis/build/; git clone --depth 1 https://github.com/NREL/SAM NREL/SAM

# Install Ruby
RUN apt-get update
RUN apt-add-repository -y ppa:brightbox/ruby-ng
RUN wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | sudo apt-key add -
RUN apt-add-repository -y 'deb https://apt.kitware.com/ubuntu/ xenial main'
RUN apt-get update
RUN apt-get install ruby2.4 ruby-switch ruby2.4-dev cmake
RUN ruby-switch --set ruby2.4

RUN gem install bundler
RUN cd /home/travis/builds/; git clone --depth 1 https://github.com/travis-ci/travis-build; git clone --depth 1 https://github.com/travis-ci/travis-support; git clone --depth 1 https://github.com/travis-ci/travis-github_apps; git clone --depth 1 https://github.com/travis-ci/travis-rollout
RUN cd /home/travis/builds/travis-build; mkdir -p ~/.travis; ln -s $PWD ~/.travis/travis-build; bundle install --gemfile ~/.travis/travis-build/Gemfile; bundler binstubs travis
RUN cd /home/travis/builds/travis-support; gem build travis-support.gemspec; gem install travis-support;
RUN cd /home/travis/builds/travis-github_apps; gem build travis-github_apps.gemspec; gem install travis-github_apps;
RUN cd /home/travis/builds/travis-rollout; gem build travis-rollout.gemspec; gem install travis-rollout;

RUN cd /home/travis/build/NREL/SAM; travis compile > /home/travis/build/test.sh