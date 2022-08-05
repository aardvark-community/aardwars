@echo off
SETLOCAL
PUSHD %~dp0
dotnet tool restore
dotnet paket restore
dotnet adaptify --local %~dp0\src\Aardwars\aardwars.fsproj
dotnet build -c Release Aardwars.sln