"""Contains functions for pulling data from Bigquery into pandas dataframes for modeling"""
import pandas as pd
import numpy as np


def smartrack_data_create(client, bqstorageclient, size):
    """Returns full data, ready for splitting and clustering"""
    desired = size
    full_query = """
        SELECT *
        FROM Mreit.Smartrack_Data_Clean  
        """

    medium_query = """
            SELECT *
            FROM Mreit.Smartrack_Data_Clean  
            LIMIT 2500000  
            """

    small_query = """
            SELECT *
            FROM Mreit.Smartrack_Data_Clean    
            LIMIT 250000
            """
    xsmall_query = """
                SELECT *
                FROM Mreit.Smartrack_Data_Clean    
                LIMIT 2500
                """
    no_null_query = """
            SELECT *
            FROM Mreit.Smartrack_Data_Clean
            WHERE CustomerID IS NOT NULL
            """
    if desired == "full":
        commit_query_dataframe = (
            client.query(full_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)
        )
    elif desired == "medium":
        commit_query_dataframe = (
            client.query(medium_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)
        )
    elif desired == "small":
        commit_query_dataframe = (
            client.query(small_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)
        )
    elif desired == "xsmall":
        commit_query_dataframe = (
            client.query(xsmall_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)
        )
    elif desired == "no_null_customers":
        commit_query_dataframe = (
            client.query(no_null_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)

        )
    else:
        print("Please enter either 'full', 'medium', or 'small' to receive data with either")
        print("ALL data, 2.5Million rows or 250K rows. You maye also enter 'no_null_customers' to")
        print("receive only rows where CustomerID is not null. Thank you")

    return commit_query_dataframe


def journey_data_create(client, bqstorageclient, size):
    """Returns full data, ready for splitting and clustering"""
    desired = size
    full_query = """
        SELECT *
        FROM Mreit.Mreit_Data_Clean  
        """

    medium_query = """
            SELECT *
            FROM Mreit.Mreit_Data_Clean   
            LIMIT 2500000  
            """

    xsmall_query = """
            SELECT *
            FROM Mreit.Mreit_Data_Clean     
            LIMIT 2500
            """
    small_query = """
                SELECT *
                FROM Mreit.Mreit_Data_Clean     
                LIMIT 250000
                """

    if desired == "full":
        commit_query_dataframe = (
            client.query(full_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)
        )
    elif desired == "medium":
        commit_query_dataframe = (
            client.query(medium_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)
        )
    elif desired == "small":
        commit_query_dataframe = (
            client.query(small_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)
        )
    elif desired == "xsmall":
        commit_query_dataframe = (
            client.query(xsmall_query)
                .result()
                .to_dataframe(bqstorage_client=bqstorageclient)
        )
    else:
        print("Please enter either 'full', 'medium', or 'small' to receive data with either")
        print("ALL data, 2.5Million rows or 250K rows. Thank you")

    return commit_query_dataframe


