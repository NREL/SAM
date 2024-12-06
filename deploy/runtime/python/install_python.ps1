 param (
    [Parameter(Mandatory=$true)][string]$version,
    [Parameter(Mandatory=$true)][string]$config
 )

$PYTHON_CONFIG_FILE = "python_config.json"

function Invoke-CommandExitOnError {
    param([string]$command)
    Write-Debug "Run command [$command]"

    $command_args = -split $command
    $exec = $command_args[0]
    if ($command_args.Count -gt 1) {
        $args = $command_args[1..($command_args.Count - 1)]
    } else {
        $args = @()
    }

    $result = Start-Process $exec -ArgumentList $args -Wait -NoNewWindow -PassThru -RedirectStandardOutput nul
    if ($result.ExitCode -ne 0) {
        $msg = "failed to run [$command]: ExitCode={0}" -f $result.ExitCode
        Write-Error $msg
        exit $result.ExitCode
    }
}


function Invoke-WebRequestExitOnError {
    param([string]$url, [string]$filename)
    Write-Debug "Invoke-WebRequest $url"

    try {
        Invoke-WebRequest -OutFile $filename $url
    }
    catch {
        throw "failed to download $url"
    }
}


function Get-Python {
    param([string]$fullVersion, [string]$majorMinor)

    $origDir = (Get-Location).Path
    $filename = "python-${fullVersion}-embed-amd64.zip"
    $url = "https://www.python.org/ftp/python/${fullVersion}/${filename}"
    if (($FORCE_DOWNLOAD -eq 1) -and (Test-Path $filename)) {
        Remove-Item $filename
    }
    if (!(Test-Path $filename)) {
        Invoke-WebRequestExitOnError $url $filename
    }

    $pythonPath = Join-Path $origDir "python-${fullVersion}"
    if (Test-Path $pythonPath) {
        Remove-Item -Recurse $pythonPath
    }
    $pythonPath = New-Item -Path $origDir -Name "python-${fullVersion}" -ItemType directory
    $filePath = Join-Path $origDir $filename
    $pythonStdLibDir = Join-Path $pythonPath "python-${majorMinor}"
    $pythonStdLibArchive = Join-Path $pythonPath "python${majorMinor}.zip"

    Set-Location $pythonPath

    Expand-Archive $filePath -DestinationPath .
    Expand-Archive $pythonStdLibArchive
    Remove-Item $pythonStdLibArchive

    if ($FORCE_DOWNLOAD -eq 1) {
        Remove-Item $filePath
    }
    Fix-PythonPath $pythonPath $majorMinor
    Get-Pip $pythonPath
    Set-Location $origDir
    return $pythonPath.name
}


function Get-Pip {
    param([String]$path)

    $filename = "get-pip.py"
    if (($FORCE_DOWNLOAD -eq 1) -and (Test-Path $filename)) {
        Remove-Item $filename
    }
    if (!(Test-Path $filename)) {
#Specific to Python 3.7 and LandBOSSE 2.3.0.3
        Invoke-WebRequestExitOnError https://bootstrap.pypa.io/pip/3.7/${filename} $filename
    }
    Invoke-CommandExitOnError ".\python.exe $filename --prefix=`"$path`" --no-warn-script-location"
    if ($FORCE_DOWNLOAD -eq 1) {
        Remove-Item $filename
    }
}


function Fix-PythonPath {
    param([string]$pythonPath, [string]$majorMinor)

    $text = @"
.
.\python$majorMinor
.\Lib
.\Lib\site-packages
"@

    $filename = Join-Path $pythonPath "python${majorMinor}._pth"
    if (Test-Path $filename) {
        Remove-Item $filename
    }
    $unused = New-Item -ItemType file $filename
    Add-Content $filename $text
    Write-Debug "Set Python path in $filename"
}


### MAIN ###
#
# Example usage:
# .\install_python.ps1 -version 3.7.4 -config .\runtime\python
#
# To test the install run this command:
# .\python\python-3.7.4\python.exe --version
#
# To enable debug prints run this in the shell:
# $DebugPreference="Continue"
#
# To prevent re-download of the Python package set the environment variable
# FORCE_DOWNLOAD to 0.
#
# If you get the error "running scripts is disabled on this system" then follow
# the provided link or run the command below to change the security policy for
# the current shell:
# Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope Process

if (Test-Path env:FORCE_DOWNLOAD) {
    $FORCE_DOWNLOAD = $env:FORCE_DOWNLOAD
} else {
    $FORCE_DOWNLOAD = 1
}
Write-Debug "FORCE_DOWNLOAD=${FORCE_DOWNLOAD}"

$ErrorActionPreference = "Stop"

$versionArray = $version -split "\."
if ($versionArray.Count -ne 3) {
    Write-Output "version must follow format x.y.z"
    exit 1
}
$majorMinor = $versionArray[0] + $versionArray[1]
Write-Debug "$version $majorMinor $config"

$orig = Get-Location
if (!(Test-Path $config)) {
    Write-Error "config path $config does not exist"
    exit 1
}

Set-Location $config
if (!(Test-Path $PYTHON_CONFIG_FILE)) {
    Write-Error "config file $PYTHON_CONFIG_FILE does not exist"
    exit 1
}

$pythonPath = Get-Python $version $majorMinor
$execPath = Join-Path $pythonPath python.exe
$pipPath = Join-Path $pythonPath Scripts | Join-Path -ChildPath pip3.exe

# Update the python config file.
$data = Get-Content -Raw -Path $PYTHON_CONFIG_FILE | ConvertFrom-Json
$data.python_version = $version
$data.exec_path = $execPath
$data.pip_path = $pipPath
ConvertTo-Json $data  | Out-File -Encoding UTF8 $PYTHON_CONFIG_FILE
Set-Location $orig
