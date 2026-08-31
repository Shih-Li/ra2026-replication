######################################################################################

# PROJECT: 	Smart Pooling and Pruning
# PURPOSE:  Create dictionnary for mapping pooled policy names
# AUTHOR:		Anirudh Sankar

#######################################################################################

policy_name_mapping <- hash(key = letters, values = letters)

policy_name_mapping$POOLED_Xslope2XSMS2ORXslope3XSMS2ORXslope2XSMS3ORXslope3XSMS3 = "(No Seeds, Slopes (All), SMS (All))" #"slope_SMS"
policy_name_mapping$POOLED_Xflat3XSMS2ORXflat3XSMS3 = "(No Seeds, High Flats, SMS (All))" #"highflat_SMS"
policy_name_mapping$POOLED_Xslope3XSMS2ORXslope3XSMS3 = "(No Seeds, High Slopes, SMS (All))" #"highslope_SMS"
policy_name_mapping$POOLED_Xslope3XSMS2 = "(No Seeds, High Slopes, Low SMS)" #"highslope_lowSMS"
policy_name_mapping$POOLED_Xslope3XSMS3 = "(No Seeds, High Slopes, High SMS)" #"highslope_highSMS"
policy_name_mapping$POOLED_Xflat2XSMS2ORXflat3XSMS2 = "(No Seeds, Flats (All), Low SMS)"
policy_name_mapping$POOLED_Xflat2XSMS3ORXflat3XSMS3 = "(No Seeds, Flats (All), High SMS)"
policy_name_mapping$POOLED_Xflat2XSMS2ORXflat3XSMS2ORXflat2XSMS3ORXflat3XSMS3 = "(No Seeds, Flats (All), SMS (All))"
policy_name_mapping$POOLED_XSMS3 = "(No Seeds, No Incentives, High SMS)"
policy_name_mapping$POOLED_Xflat3 = "(No Seeds, High Flat, No Reminder)"
policy_name_mapping$POOLED_Xslope3 = "(No Seeds, High Slope, No Reminder)"
policy_name_mapping$POOLED_Xslope2XSMS2 = "(No Seeds, Low Slope, Low SMS)"
policy_name_mapping$POOLED_Xslope2XSMS3 = "(No Seeds, Low Slope, High SMS)"
policy_name_mapping$POOLED_Xslope2ORXslope3 = "(No Seeds, Slope (All), No Reminder)"
policy_name_mapping$POOLED_XSMS2ORXSMS3 = "(No Seeds, No Incentive, SMS (All))"
policy_name_mapping$POOLED_XSMS2 = "(No Seeds, No Incentive, Low SMS)"
policy_name_mapping$POOLED_Xslope2XSMS2ORXslope2XSMS3 = "(No Seeds, Low Slope, SMS (All))"

policy_name_mapping$POOLED_trusted0Xslope2XSMS2ORtrusted0Xslope3XSMS2ORtrusted0Xslope2XSMS3ORtrusted0Xslope3XSMS3 = "(Trusted Seeds, Slopes (All), SMS (All))" #"trusted_slope_SMS"
policy_name_mapping$POOLED_trusted0Xflat2XSMS3ORtrusted0Xflat3XSMS3 = "(Trusted Seeds, Flats (All), High SMS)"
policy_name_mapping$POOLED_trusted0Xslope3XSMS2ORtrusted0Xslope3XSMS3 = "(Trusted Seeds, High Slope, SMS (All))" #"trusted_highslope_SMS"
policy_name_mapping$POOLED_trusted0XSMS2ORtrusted0XSMS3 = "(Trusted Seeds, No Incentive, SMS (All))" #"trusted_highslope_SMS"
policy_name_mapping$POOLED_trusted0Xslope2XSMS2ORtrusted0Xslope2XSMS3 = "(Trusted Seeds, Low Slope, SMS (All))"
policy_name_mapping$POOLED_trusted0Xflat2XSMS2ORtrusted0Xflat3XSMS2 = "(Trusted Seeds, Flats (All), Low SMS)"
policy_name_mapping$POOLED_trusted0Xslope3XSMS2 = "(Trusted Seeds, High Slope, Low SMS)"

policy_name_mapping$POOLED_gossip2Xslope2XSMS2ORgossip3Xslope2XSMS2ORgossip2Xslope3XSMS2ORgossip3Xslope3XSMS2ORgossip2Xslope2XSMS3ORgossip3Xslope2XSMS3ORgossip2Xslope3XSMS3ORgossip3Xslope3XSMS3 = "(Info Hubs (All), Slopes (All), SMS (All))" #"gossipPooled_slope_SMS"
policy_name_mapping$POOLED_gossip2Xflat2ORgossip3Xflat2ORgossip2Xflat3ORgossip3Xflat3 = "(Info Hubs (All), Flats (All), No Reminder)" #"gossipPooled_flat"
policy_name_mapping$POOLED_gossip2XSMS2ORgossip3XSMS2ORgossip2XSMS3ORgossip3XSMS3 = "(Info Hubs (All), No Incentives, SMS (All))" #"gossipPooled_SMS"
policy_name_mapping$POOLED_gossip2Xslope2XSMS2ORgossip2Xslope3XSMS2ORgossip2Xslope2XSMS3ORgossip2Xslope3XSMS3 = "(Info Hubs, Slopes (All), SMS (All))" #"gossip_slope_SMS"
policy_name_mapping$POOLED_gossip2Xslope3XSMS2ORgossip3Xslope3XSMS2ORgossip2Xslope3XSMS3ORgossip3Xslope3XSMS3 = "(Info Hubs (All), High Slopes, SMS (All))"
policy_name_mapping$POOLED_gossip2Xslope3 = "(Info Hubs, High Slope, No Reminder)"
policy_name_mapping$POOLED_gossip2XSMS2 = "(Info Hubs, No Incentives, Low SMS)"
policy_name_mapping$POOLED_gossip2ORgossip3 = "(Info Hubs (All), No Incentives, No Reminder)"
policy_name_mapping$POOLED_gossip2Xflat3ORgossip3Xflat3 = "(Info Hubs (All), High Flat, No Reminder)"
policy_name_mapping$POOLED_gossip2Xflat2ORgossip3Xflat2 = "(Info Hubs (All), Low Flat, No Reminder)"
policy_name_mapping$POOLED_gossip2XSMS3 = "(Info Hubs, No Incentive, High SMS)"
policy_name_mapping$POOLED_gossip2Xflat2ORgossip2Xflat3 = "(Info Hubs, Flats (All), No Reminder)"
policy_name_mapping$POOLED_gossip2Xslope2 =  "(Info Hubs, Low Slope, No Reminder)"
policy_name_mapping$POOLED_gossip2XSMS2ORgossip3XSMS2ORgossip2XSMS3 = "(Info Hubs (All), No Incentive, SMS (All))" ## THIS ONE IS WEIRD... MISSING gossip3XSMS3
policy_name_mapping$POOLED_gossip2Xslope2ORgossip3Xslope2ORgossip2Xslope3ORgossip3Xslope3 = "(Info Hubs (All), Slopes (All), No Reminder)"
policy_name_mapping$POOLED_gossip2Xflat3XSMS2ORgossip3Xflat3XSMS2ORgossip2Xflat3XSMS3ORgossip3Xflat3XSMS3  = "(Info Hubs (All), High Flats, SMS (All))"
policy_name_mapping$POOLED_gossip2Xslope2XSMS2ORgossip3Xslope2XSMS2ORgossip2Xslope2XSMS3ORgossip3Xslope2XSMS3 = "(Info Hubs (All), Low Slope, SMS (All))"

policy_name_mapping$POOLED_gossip3Xslope2XSMS2ORgossip3Xslope3XSMS2ORgossip3Xslope2XSMS3ORgossip3Xslope3XSMS3 = "(Trusted Info Hubs, Slopes (All), SMS (All))" #"trustgossip_slope_SMS"
policy_name_mapping$POOLED_gossip3Xflat3XSMS2ORgossip3Xflat3XSMS3 = "(Trusted Info Hubs, High Flat, SMS (All))"
policy_name_mapping$POOLED_gossip3Xslope2 = "(Trusted Info Hubs, Low Slope, No Reminder)"
policy_name_mapping$POOLED_gossip3Xslope3 = "(Trusted Info Hubs, High Slope, No Reminder)"
policy_name_mapping$POOLED_gossip3Xflat2 = "(Trusted Info Hubs, Low Flat, No Reminder)"
policy_name_mapping$POOLED_gossip3Xflat3 = "(Trusted Info Hubs, No Incentive, No Reminder)"
policy_name_mapping$POOLED_gossip3XSMS2 = "(Trusted Info Hubs, No Incentive, No Reminder)"
policy_name_mapping$POOLED_gossip3XSMS3 = "(Trusted Info Hubs, No Incentive, High SMS)"

policy_name_mapping$POOLED_random0Xslope2XSMS2ORrandom0Xslope3XSMS2ORrandom0Xslope2XSMS3ORrandom0Xslope3XSMS3 = "(Random Seeds, Slopes (All), SMS (All))" #"random_slope_SMS"
policy_name_mapping$POOLED_random0 = "(Random Seeds, No Incentive, No Reminder)"
policy_name_mapping$POOLED_random0XSMS2ORrandom0XSMS3 = "(Random Seeds, No Incentive, SMS (All))"
policy_name_mapping$POOLED_random0Xflat2XSMS2ORrandom0Xflat3XSMS2ORrandom0Xflat2XSMS3ORrandom0Xflat3XSMS3 = "(Random Seeds, Flats (All), SMS (All))"


                                                                      
                                                                         
    

######################################################################################
#
# END
#
#######################################################################################                                                      
                                                                         
                                                                         
                                                                         
  




 
