## GUI installer for the AWS SDK for SAP ABAP

Latest: [/awslabs/sdk_installer.prog.abap](https://github.com/awslabs/gui-installer-for-abap-sdk/blob/main/src/%23awslabs%23sdk_installer.prog.abap)

This project is about creating a standalone SAP ABAP report for streamlining the installation and maintenance of the [AWS SDK for SAP ABAP](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/home.html). In order to provide a plug 'n' play experience, the report was designed to be installed on compatible ABAP-based system via simply <b>copying & pasting</b> the latest version into a corresponding report on a user's SAP system. The report offers a graphical user interface and automation functions, which help with:

- Downloading the required Amazon root certificates from [Amazon trust services](https://www.amazontrust.com/repository/) and adding them to STRUST
- Downloading the AWS SDK for SAP ABAP [zip](https://sdk-for-sapabap.aws.amazon.com/awsSdkSapabapV1/release/abapsdk-LATEST.zip) files
- Installing selected transports for desired SDK modules
- Updating installed SDK modules
- Deleting no longer needed SDK modules
- Browsing through installed and available modules in a comprehensive fashion
- Check on individual modules' transport log, object list, and API [documentation](https://docs.aws.amazon.com/sdk-for-sap-abap/v1/api/latest/index.html)

![image](https://github.com/user-attachments/assets/35213190-76c5-4319-ab64-3094170b67ca)

## How to use 

Create a new report in ADT / SE80 / SE38 on an SAP system where you want to use the GUI installer, give it a valid name in a valid namespace (Z | Y | whatever you have access to), <b>copy & paste</b> the source code from the latest build into it and hit execute. The report gives an overview of the ABAP SDK modules currently installed on your SAP system and their version. If there is a more current module available, it will indicate accordingly. Furthermore, the report will show you a list of all currently available ABAP SDK modules, which are not yet installed on your system.

## Current limitations
- Only supports NetWeaver 7.4 and higher based SAP systems (same [prerequisite](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/prerequisites.html#sdk) as the AWS SDK for SAP ABAP)
- Needs the SAP system to have a working Internet connection for downloading ABAP SDK [json](https://sdk-for-sapabap.aws.amazon.com/awsSdkSapabapV1/release/abapsdk-LATEST.json) and [zip](https://sdk-for-sapabap.aws.amazon.com/awsSdkSapabapV1/release/abapsdk-LATEST.zip) files
- Currently needs manual updating by copy/pasting the latest version
- No support for the [BTP edition of the AWS SDK for SAP ABAP](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/installation-btp.html), yet

## Security

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## License

This project is licensed under the Apache-2.0 License.

