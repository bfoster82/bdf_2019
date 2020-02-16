"""This serves as a template for any script that will need to pull data from BigQuery"""

from google.cloud import bigquery
from google.oauth2 import service_account
from google.cloud import bigquery_storage_v1beta1
import citybus_data_create as city

"""*Before altering this script in any way, please save it under a different filename so the template remains as-is*"""


"""Change the filename in the credentials object to match the one Jacob sent you. 
Make sure it is in your project directory. You may need to change your working directory, depending on what environment
you are using."""

credentials = service_account.Credentials.from_service_account_file(
    'citybus-cred.json')


"""Do not alter or delete any text from here to next comment"""

project_id = 'capstone-498-citybus'

client = bigquery.Client(credentials=credentials, project=project_id)
bigquery_dataset = client.dataset('Mreit')

bqstorageclient = bigquery_storage_v1beta1.BigQueryStorageClient(
    credentials=credentials
)
"""Do not alter or delete any text above this comment"""


# Function Instructions:

"""
Instructions for pulling Smartrack data:

To use the functions for pulling data, simply copy the code below and assign it to the data object named however you
wish.

Leave 'client' and 'bqstorageclient' as-is, and change the thirs argument to one of the following:
* "small" will return the top 250,000 rows
* "medium" will return the top 2.5 Million rows
* "full" will return all data (17 Million rows)
* "no_null_customers" will return only rows where CustomerID is not null (4.5 Million rows)

YOUR_OBJECT_NAME_HERE = city.smartrack_data_create(client, bqstorageclient, "small")


"""

"""
Instructions for pulling Journey (Mreit) data:

This function works the same as the Smartrack function, but does not include the 'no_null_customers' option.

Leave 'client' and 'bqstorageclient' as-is, and change the thirs argument to one of the following:
* "small" will return the top 250,000 rows
* "medium" will return the top 2.5 Million rows
* "full" will return all data (52 Million rows)

YOUR_OBJECT_NAME_HERE = city.journey_data_create(client, bqstorageclient, "small")


"""
# Feel free to delete the below code once you run things once to make sure it works
smart_data_test = city.smartrack_data_create(client, bqstorageclient, "small")
print(smart_data_test.columns)
print(len(smart_data_test))


journey_data_test = city.journey_data_create(client, bqstorageclient, "small")
print(journey_data_test.columns)
print(len(journey_data_test))



