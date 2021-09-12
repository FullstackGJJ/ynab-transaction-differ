## CellRowParser domain

This domain is responsible for parsing a delimited row into cell row which has an extra guarantee of being valid

Mental Models:

- A delimited row is a string that contains information structured as a row string with its columns separated by delimiters
- A cell is a mental model that holds information about a column
- An empty cell represents when there is nothing between delimiters
- A filled cell represents where there is something between delimiters
- A cell row is a list of cells

The rules of thumb are:

- The delimited row should contain atleast one instance of its delimiter

An expert ConfigurableRowParser should always be able to:

- Convert a delimited row into a cell row
- Tell when a cell is filled or empty

An expert BankAccountTransactionParser under the right conditions can:
