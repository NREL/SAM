
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
    param([string]$fullVersion)
    $version = $fullVersion -split "\."
    if ($version.Count -ne 3) {
        Write-Output "version must follow format x.y.z"
        exit 1
    }

    $origDir = (Get-Location).Path
    $majorMinor = $version[0] + $version[1]
    $filename = "python-" + $fullVersion + "-embed-amd64.zip"
    $url = "https://www.python.org/ftp/python/" + $fullVersion + "/" + $filename
    Invoke-WebRequestExitOnError $url $filename

    $pythonPath = $origDir + "\python-" + $fullVersion
    $filePath = $origDir + "\" + $filename
    $pythonStdLibDir = "python" + $majorMinor 
    $pythonStdLibArchive = $pythonPath + "\$pythonStdLibDir" + ".zip"

    Invoke-CommandExitOnError "mkdir $pythonPath"
    Set-Location $pythonPath
    Invoke-CommandExitOnError "unzip -o $filePath"
    Invoke-CommandExitOnError "rm $filePath"
    Invoke-CommandExitOnError "mkdir $pythonStdLibDir"
    Set-Location $pythonStdLibDir
    Invoke-CommandExitOnError "unzip -o $pythonStdLibArchive"
    Invoke-CommandExitOnError "rm $pythonStdLibArchive"
    Fix-PythonPath $pythonPath $PYTHON_MAJOR_MINOR
    Set-Location $pythonPath
    Get-Pip $pythonPath
    Set-Location $origDir
    return $pythonPath
}


function Get-Pip {
    param([string]$pythonPath)
    Write-Debug "download pip to pythonPath=$pythonPath"
    $origDir = (Get-Location).Path
    Set-Location $pythonPath
    Invoke-WebRequestExitOnError https://bootstrap.pypa.io/get-pip.py get-pip.py
    
    Invoke-CommandExitOnError ".\python.exe get-pip.py --prefix=$pythonPath --no-warn-script-location"
    Invoke-CommandExitOnError "rm get-pip.py"
    Set-Location $origDir
}


function Fix-PythonPath {
    param([string]$pythonPath, [string]$majorMinor)

    $text = @"
$pythonPath
$pythonPath\python$majorMinor
$pythonPath\Lib
$pythonPath\Lib\site-packages
"@

    $filename = $pythonPath + "\python" + $majorMinor + "._pth"
    rm $filename
    touch $filename
    Add-Content $filename $text
    Write-Debug "Set Python path in $filename"
}


### MAIN ###
#
# This should be executed in PowerShell in the sam_dev directory.
# To test the install run this command:
# .\python\python-3.7.4\Scripts\pip.exe install pandas
#
# To enable debug prints run this in the shell:
# $DebugPreference="Continue"
#
# If you get the error "running scripts is disabled on this system" then follow
# the provided link or run the command below to change the security policy for
# the current shell:
# Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope Process

# TODO: Allow user to specify the python version.

$PYTHON_FULL_VERSION = "3.7.4"
$PYTHON_MAJOR_MINOR = "37"

$orig = (Get-Location).Path
$basePath = "python"
if (!(Test-Path $basePath)) {
    New-Item -Path . -Name python -ItemType "directory"
}
Set-Location $basePath
Get-Python $PYTHON_FULL_VERSION
Set-Location $orig
