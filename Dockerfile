# Base image with R 4.5.2
FROM r-base:4.5.2

# Set working directory
WORKDIR /rlforaging

# Install system dependencies
RUN apt update && apt install -y \
    make \
    cmake \
    build-essential \
    libssl-dev \
    zlib1g-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    curl \
    libcurl4-openssl-dev \
    git \
    libncursesw5-dev \
    xz-utils tk-dev \
    libxml2-dev \
    libxmlsec1-dev \
    libffi-dev \
    liblzma-dev \
    libcairo2-dev \
    libxt-dev

# Install and manage python using pyenv
ENV PYENV_ROOT="/root/.pyenv"
ENV PATH="$PYENV_ROOT/bin:$PYENV_ROOT/shims:$PATH"
RUN curl -fsSL https://pyenv.run | bash && \
    pyenv install 3.13.9 && \
    pyenv global 3.13.9

# Install python dependencies
COPY requirements.txt ./requirements.txt
RUN python -m venv venv && \
    ./venv/bin/pip install --upgrade pip && \
    ./venv/bin/pip install -r requirements.txt

# Install and mange R packages using renv
# Due to a bug in an older version of renv, the /etc/os-release my need to be modified for renv::restore() to work (issue 2197 on GitHub https://github.com/rstudio/renv).
RUN echo 'VERSION_ID="unknown"' >> /etc/os-release

# Install and initialize renv
RUN R -e "install.packages('renv', repos = 'https://cran.rstudio.com')"
RUN R -s -e "renv::init(bare=TRUE)"  

# Pre-install StanHeaders and RcppParallel + RcppEigen to avoid build issues.
RUN R -e 'install.packages(c("RcppParallel", "RcppEigen"))'
RUN R -e 'install.packages("StanHeaders")'

# Copy R dependency files and restore packages
COPY renv.lock ./renv.lock
RUN R -s -e "renv::restore()"

# Install cmdstan
RUN R -e 'cmdstanr::install_cmdstan()'

# Copy Stan source files and compile in a dedicated layer.
COPY code/rl/alone/stan ./code/rl/alone/stan
COPY code/rl/catches/stan ./code/rl/catches/stan
COPY code/rl/nocatches/stan ./code/rl/nocatches/stan
COPY code/compile_stan.R ./code/compile_stan.R
RUN Rscript code/compile_stan.R

# Copy remaining project files. 
COPY .here ./.here
COPY .renvignore ./.renvignore
COPY code ./code

# Run the main.R script
CMD ["Rscript", "code/main.R"]