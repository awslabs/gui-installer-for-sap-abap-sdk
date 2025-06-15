## GUI installer for the AWS SDK for SAP ABAP

Latest: [/awslabs/sdk_installer.prog.abap](https://github.com/awslabs/gui-installer-for-abap-sdk/blob/main/src/%23awslabs%23sdk_installer.prog.abap)

## How to use 

Open a new report in ADT / SE80 / SE38 on the SAP system where you want to use the GUI installer, give it a valid name in a valid namespace (Z | Y | whatever you have access to), copy & paste the source code from the latest build into it and hit execute. The report gives an overview of the ABAP SDK modules currently installed on your SAP system and their version. If there is a more current module available, the report will indicate accordingly. Furthermore, the report will show you a list of all currently available ABAP SDK modules, which are not yet installed on your system.

![image](https://github.com/user-attachments/assets/35213190-76c5-4319-ab64-3094170b67ca)

## Current limitations
- Only supports NetWeaver 7.4 and higher based SAP systems (same [prerequisite](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/prerequisites.html#sdk) as the AWS SDK for SAP ABAP)
- Needs the SAP system to have a working Internet connection for downloading ABAP SDK [json](https://sdk-for-sapabap.aws.amazon.com/awsSdkSapabapV1/release/abapsdk-LATEST.json) and [zip](https://sdk-for-sapabap.aws.amazon.com/awsSdkSapabapV1/release/abapsdk-LATEST.zip) files
- Currently needs manual updating by copy/pasting the latest version
- No support for the [BTP edition of the AWS SDK for SAP ABAP](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/installation-btp.html), yet

## Security

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## License

This project is licensed under the Apache-2.0 License.

