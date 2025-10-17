# Changelog

## Development Build: equuleus-rc1+dev54
- 'Fix #237, update sample app to reflect 582 standard'
- See: <https://github.com/nasa/{{app_name_lc}}/pull/238>


## Development Build: equuleus-rc1+dev50
- Address sanitizer issue
- See <https://github.com/nasa/{{app_name_lc}}/pull/235>

## Development Build: equuleus-rc1+dev46
- Convert remaining int32 CFE status variables to CFE_Status_t
- Convert syslog writes during initialization to events
- See <https://github.com/nasa/{{app_name_lc}}/pull/218> and <https://github.com/nasa/{{app_name_lc}}/pull/216>

## Development Build: equuleus-rc4+dev40
- apply name changes to EDS dispatcher
- See <https://github.com/nasa/{{app_name_lc}}/pull/229>

## Development Build: equuleus-rc1+dev36
- updating {{app_name_lc}} to use new versioning system
- See <https://github.com/nasa/{{app_name_lc}}/pull/226>

## Development Build: v1.3.0-rc4+dev69
- define msgids via topicids
- See <https://github.com/nasa/{{app_name_lc}}/pull/220>

## Development Build: v1.3.0-rc4+dev65
- bring {{app_name_lc}} fully into compliance
- Rename CommandCode variable to FcnCode
- Add check for success of CFE_TBL_Load() during Initialization
- See <https://github.com/nasa/{{app_name_lc}}/pull/212>, <https://github.com/nasa/{{app_name_lc}}/pull/187>, and <https://github.com/nasa/{{app_name_lc}}/pull/190>

## Development Build: v1.3.0-rc4+dev56
- Apply consistent Event ID names to common events
- Remove component-specific cFE header #includes
- Refactor {{app_name_uc}}_Init/Process to remove multiple returns
- Add test for missing branch in {{app_name_uc}}_Process()
- Zero out global data structure during initialization
- Move cmds and utils into separate files
- organize source files according to current patterns
- See <https://github.com/nasa/{{app_name_lc}}/pull/189>, <https://github.com/nasa/{{app_name_lc}}/pull/195>, <https://github.com/nasa/{{app_name_lc}}/pull/198>, <https://github.com/nasa/{{app_name_lc}}/pull/200>, <https://github.com/nasa/{{app_name_lc}}/pull/201>, <https://github.com/nasa/{{app_name_lc}}/pull/205>, and <https://github.com/nasa/{{app_name_lc}}/pull/208>

## Development Build: v1.3.0-rc4+dev39
- update cmake recipe
- See <https://github.com/nasa/{{app_name_lc}}/pull/203>

## Development Build: v1.3.0-rc4+dev35
- Remove redundant comments
- Create CHANGELOG.md
- See <https://github.com/nasa/{{app_name_lc}}/pull/185> and <https://github.com/nasa/{{app_name_lc}}/pull/183>

## Development Build: v1.3.0-rc4+dev27
- Misaligned comments
- Remove unnecessary parentheses around return values.
- Remove 'return;' from last line of void functions.
- See <https://github.com/nasa/{{app_name_lc}}/pull/177>, <https://github.com/nasa/{{app_name_lc}}/pull/179>, and <https://github.com/nasa/{{app_name_lc}}/pull/181>

## Development Build: v1.3.0-rc4+dev21
- Remove registration of empty event filters
- Update codeql workflow for reusable updates
- See <https://github.com/nasa/cFS/pull/505> 

## Development Build: v1.3.0-rc4+dev16
- Update copyright headers
- Standardize version information
- See <https://github.com/nasa/{{app_name_lc}}/pull/171> and <https://github.com/nasa/cFS/pull/445>

## Development Build: v1.3.0-rc4+dev9
- Use preferred UT patterns
- Reuse CodeQL, Static Analysis, and Format Check
- See <https://github.com/nasa/cFS/pull/414>

## Development Build: v1.3.0-rc4+dev4
- Use CFE_MSG_PTR conversion macro
- Update baseline for cFS-Caelum-rc4 to v1.3.0-rc4
- See <https://github.com/nasa/{{app_name_lc}}/pull/163> and <https://github.com/nasa/cFS/pull/390>

## Development Build: v1.2.0-rc1+dev73
- Apply CFE_SB_ValueToMsgId where required
- See <https://github.com/nasa/{{app_name_lc}}/pull/155> and <https://github.com/nasa/cFS/pull/359>

## Development Build: v1.2.0-rc1+dev66
-  Implement Coding Standard in CodeQL workflow
- See <https://github.com/nasa/{{app_name_lc}}/pull/150> and <https://github.com/nasa/cFS/pull/270>

## Development Build: v1.2.0-rc1+dev62
- Removes app registration call, `CFE_ES_RegisterApp()` since applications do not need to register themselves.
- Apply standard header guard on all header files by removing leading underscore. Convert file-scope block comments to doxygen format.
- See <https://github.com/nasa/{{app_name_lc}}/pull/145>

## Development Build: v1.2.0-rc1+dev56
- Replaces <> with " in local includes
- Adds CONTRIBUTING.md that links to the main cFS contributing guide.
- Adds a description for the requirements of command and telemetry Message IDs to explain why the Msg IDs have those requirements in documentation.
- See <https://github.com/nasa/{{app_name_lc}}/pull/137>

## Development Build: v1.2.0-rc1+dev48
- Simplify build to use wrappers and interface libs
- Add Testing Tools to the Security Policy
- See <https://github.com/nasa/{{app_name_lc}}/pull/130>

## Development Build: v1.2.0-rc1+dev37
- Documentation: Add `Security.md` with instructions on reporting vulnerabilities
- Resolves bug where success code was reported as an error for `CFE_TBL_GetAddress`.
- Rename `UT_ClearForceFail` as `UT_ClearDefaultValue` given change from <https://github.com/nasa/osal/issues/724>
- See <https://github.com/nasa/{{app_name_lc}}/pull/121>

## Development Build: v1.2.0-rc1+dev29
- Aligns messages according to changes in cFE <https://github.com/nasa/cFE/issues/1009>. Uses the "raw" message cmd/tlm types in definition
- See <https://github.com/nasa/{{app_name_lc}}/pull/114>

## Development Build: v1.2.0-rc1+dev25
- Rename `UT_SetForceFail` to `UT_SetDefaultReturnValue` since some functions that retain more than 1 value are not necessarily failing
- See <https://github.com/nasa/{{app_name_lc}}/pull/113>

## Development Build: v1.2.0-rc1+dev22
- Replaces deprecated SB API's with MSG
- No impact, removes undesirable pattern use of `OS_PACK`
- See <https://github.com/nasa/{{app_name_lc}}/pull/108>

## Development Build: v1.2.0-rc1+dev18
- No behavior changes. All identifiers now use the prefix `{{app_name_uc}}_`. Changes the name of the main function from SAMPLE_AppMain to {{app_name_uc}}_Main which affects the CFE startup script.
- Set REVISION to "99" to indicate development version status
- See <https://github.com/nasa/{{app_name_lc}}/pull/102>

## Development Build: v1.2.0-rc1+dev13
- Unit test MID string format now 32bit
- Installs unit test to target directory
- Checks only format string in UT event test
- See <https://github.com/nasa/{{app_name_lc}}/pull/98>

## Development Build: v1.2.0-rc1+dev5
- Applies standard coding style.
- Removes test code call of CFE_SB_InitMsg and sets the API/stub buffers directly.
- See <https://github.com/nasa/{{app_name_lc}}/pull/93>

## Development Build: 1.1.0+dev65
- Add build number and baseline to version report
- Install unit test as part of cmake recipe. Sample app test runner now shows up in expected install directory
- See <https://github.com/nasa/{{app_name_lc}}/pull/86>

## Development Build: 1.1.11
- Move the table to fsw/tables and renames "sample_table" to "{{app_name_lc}}_table
- See <https://github.com/nasa/{{app_name_lc}}/pull/76>

## Development Build: 1.1.10
- Test cases now compare an expected event string with a string derived from the spec string and arguments that were output by the unit under test.
- Replace references to `ccsds.h` types with the `cfe_sb.h`-provided type.
- See <https://github.com/nasa/{{app_name_lc}}/pull/71>

## Development Build: 1.1.9
- Applies the CFE_SB_MsgIdToValue() and CFE_SB_ValueToMsgId() routines where compatibility with an integer MsgId is necessary - syslog prints, events, compile-time MID #define values.
- No more format conversion error in RTEMS build
- See <https://github.com/nasa/{{app_name_lc}}/pull/63>

## Development Build: 1.1.8
- Coverage data from make lcov includes the {{app_name_lc}} code
- See <https://github.com/nasa/{{app_name_lc}}/pull/62>

## Development Build: 1.1.7
- Fix bug where table is not released after being used
- Minor updates (see <https://github.com/nasa/{{app_name_lc}}/pull/52>)

## Development Build: 1.1.6
- Minor updates (see <https://github.com/nasa/{{app_name_lc}}/pull/49>)

## Development Build: 1.1.5
- Fix to build on RASPBIAN OS
- Minor updates (see <https://github.com/nasa/{{app_name_lc}}/pull/47>)

## Development Build: 1.1.4
- Fix for a clean build with OMIT_DEPRECATED
- Minor updates (see <https://github.com/nasa/{{app_name_lc}}/pull/44>)

## Development Build: 1.1.3
- Minor updates (see <https://github.com/nasa/{{app_name_lc}}/pull/34>)

## Development Build: 1.1.2
- Minor updates (see <https://github.com/nasa/{{app_name_lc}}/pull/20>)

## Development Build: 1.1.1
- Minor updates (see <https://github.com/nasa/{{app_name_lc}}/pull/15>)

## _**OFFICIAL RELEASE: 1.1.0 - Aquila**_
- Minor updates (see <https://github.com/nasa/{{app_name_lc}}/pull/11>)
- Not backwards compatible with OSAL 4.2.1
- Released as part of cFE 6.7.0, Apache 2.0

## _**OFFICIAL RELEASE: 1.0.0a**_
- Released as part of cFE 6.6.0a, Apache 2.0

## Known issues
As a sample application, extensive testing is not performed prior to release and only minimal functionality is included. Note discrepancies likely exist between this application and the example detailed in the application developer guide.

## Getting Help
For best results, submit issues:questions or issues:help wanted requests at <https://github.com/nasa/cFS>.

Official cFS page: <http://cfs.gsfc.nasa.gov>
