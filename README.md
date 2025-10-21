# GUI installer for the AWS SDK for SAP ABAP

Latest: [/awslabs/sdk_installer.prog.abap](https://github.com/awslabs/gui-installer-for-abap-sdk/blob/main/src/%23awslabs%23sdk_installer.prog.abap)

This project is about creating a standalone SAP ABAP report for streamlining the installation and maintenance of the [AWS SDK for SAP ABAP](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/home.html). In order to provide a plug 'n' play experience, the report was designed to be installed on compatible ABAP-based system via simply <b>copying & pasting</b> the latest version into a corresponding report on a user's SAP system. The report offers a graphical user interface and automation functions, which help with:

- Downloading the required Amazon root certificates from [Amazon trust services](https://www.amazontrust.com/repository/) and adding them to STRUST
- Downloading the required AWS SDK for SAP ABAP .zip files
- Unpacking, adding and importing transports for to be installed SDK modules
- Unpacking, adding and importing transports for updating installed SDK modules
- Deleting no longer needed SDK modules
- Browsing through installed and available modules in a comprehensive fashion
- Check on individual modules' transport logs, object lists, and respective API documentation

![image](https://github.com/user-attachments/assets/35213190-76c5-4319-ab64-3094170b67ca)

## Deploying on SAP RISE

The report can also be deployed and executed on an SAP RISE system under the following pre-requisites:

- For any Git-based installation (i.e. non copy/paste), the SAP RISE system needs to able to connect to [github.com](github.com)
- To download the ABAP SDK, connectivity form SAP RISE to [aws.amazon.com](aws.amazon.com) is required
- Amazon SSL certificates needed for connectivty have to be downloadable from [amazontrust.com](amazontrust.com)
- <b>OPTIONAL</b>: If Internet access from the SAP RISE system is happening via a proxy, the SAP RISE system’s ICM has to be configured accordingly and the above-mentioned sites need to be allow-listed on the respective proxy. At the time of this writing, they are not part of the standard allow list of a proxy shipped with a SAP RISE environment.

## How to use 

Create a new report in ADT / SE80 / SE38 on an SAP system where you want to use the GUI installer, give it a valid name in a valid namespace (Z | Y | whatever you have access to), <b>copy & paste</b> the source code from the latest build into it, <b>activate</b> and <b>hit execute</b>.

## Current limitations
- Only supports NetWeaver 7.4 and higher based SAP systems (same [prerequisite](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/prerequisites.html#sdk) as the AWS SDK for SAP ABAP)
- Needs the SAP system to have a working Internet connection for downloading ABAP SDK .json and .zip files
- Needs some disk space for the ABAP SDK .zip files on the SAP system's filesystem
- Currently needs manual updating by copy/pasting the latest version
- No support for the [BTP edition of the AWS SDK for SAP ABAP](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/installation-btp.html), yet

## Security

See [CONTRIBUTING](CONTRIBUTING.md#security-issue-notifications) for more information.

## License

This project is licensed under the Apache-2.0 License.

