# Luxury Goods Authentication System

## Overview

The Luxury Goods Authentication System is a smart contract-based solution built on the Stacks blockchain. It provides a secure and transparent way to authenticate, track, and manage luxury goods throughout their lifecycle. This system helps manufacturers, retailers, and consumers verify the authenticity of luxury items and track their ownership history.

## Features

- **Product Registration**: Manufacturers can register new luxury products with unique serial numbers.
- **Ownership Transfer**: Secure transfer of product ownership between parties.
- **Authentication**: Verify the authenticity of luxury goods using their serial numbers.
- **Ownership History**: Track the complete ownership history of each product.
- **Status Management**: Update and track the status of products (active, suspended, retired).
- **Role-based Access Control**: Separate roles for manufacturers, retailers, and contract owner.

## Smart Contract Details

The smart contract (`ori-codebase.clar`) is written in Clarity, the smart contract language for the Stacks blockchain. It includes the following main components:

- Data maps for products, manufacturers, retailers, and product history
- Public functions for product registration, ownership transfer, and status updates
- Read-only functions for product verification and history retrieval

## Setup

1. Install [Clarinet](https://github.com/hirosystems/clarinet), the Clarity development tool.
2. Clone this repository:
   ```
   git clone https://github.com/your-username/luxury-goods-authentication.git
   cd luxury-goods-authentication
   ```
3. Set up the Clarinet project:
   ```
   clarinet new
   ```

## Usage

### Deploying the Contract

To deploy the contract to the Stacks blockchain:

1. Configure your Stacks wallet in Clarinet.
2. Run the deployment command:
   ```
   clarinet deploy
   ```

### Interacting with the Contract

You can interact with the contract using Clarinet's console or by building a frontend application. Here are some example interactions:

1. Register a manufacturer:
   ```
   (contract-call? .ori-codebase register-manufacturer 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
   ```

2. Register a product:
   ```
   (contract-call? .ori-codebase register-product "SN123456" "LuxeBrand" "ExclusiveModel")
   ```

3. Transfer ownership:
   ```
   (contract-call? .ori-codebase transfer-ownership "SN123456" 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)
   ```

4. Verify a product:
   ```
   (contract-call? .ori-codebase verify-product "SN123456")
   ```

## Testing

To run the contract tests:

```
clarinet test
```

## Contributing

Contributions to the Luxury Goods Authentication System are welcome! Please follow these steps:

1. Fork the repository
2. Create a new branch for your feature
3. Commit your changes
4. Push to your branch
5. Create a new Pull Request

## Contact

For any questions or concerns, please open an issue on the GitHub repository or contact the project maintainers.
