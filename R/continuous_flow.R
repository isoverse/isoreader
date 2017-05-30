#' Load continuous flow data
#' 
#' @export
isoread_continuous_flow <- function() {
  
}

# get supported continuous flow file types and which functions they map to
get_supported_cf_files <- function() {
  tribble(
    ~extension, ~fun,            ~description,
    "cf",       isoread_cf,      "Isodat Continuous Flow file format (older)",
    "dxf",      isoread_dxf,     "Isodat Continuous Flow file format (newer)",
    "iarc",     isoread_flow_iarc, "IonOS Continous Flow data archieve" 
  )
}

#
isoread_cf <- function() {
  
}


#
isoread_dxf <- function() {
  
}


isoread_iarc <- function() {
  # useful information:
  #
  # File: Info
  #   ProcessingListId --> look into file "ProcessingList_[ProcessingListId]"
  #   Name: whole batch name --> could be useful
  #   NumberOfTasks: ~number of samples (but each may have multiple data traces)
  #   DefinitionUniqueIdentifier: {BC89E456-57C0-4FF1-9110-90C8E6AE1B69} -- also appears in ProcessingList_1 and the Task files
  # File: ProcessingList_1
  #   DefinitionUniqueIdentifier: {BC89E456-57C0-4FF1-9110-90C8E6AE1B69} -- this appears in the task files to refere back to this!
  #   First <PropertyBag> has several ChildPropertyBags
    #   <Identifier>A0A04728-4135-4E4B-8D78-AB3C8B4F634F</Identifier> - has information on isotope value labels
    #       5 subblocks with <Identifier>10DC1602-5ED4-4D62-BAB0-2693E3FBC3AF</Identifier> - species definitions for CO, O2, SO2, CO2, H2 !!! - this is what the raw data files link back to in terms of ratios
    #       useful subblocks:
    #       <Identifier>{EBCA127B-30AC-49DF-997D-24D32591A6BB}</Identifier>
    #         <Identifier>{6C3314EA-7D96-4EEA-806A-1C3433E06903}</Identifier>
    #         <Value>dH</Value><Identifier>Label</Identifier>
    #         <Value>VSMOW</Value><Identifier>Scale</Identifier>
    #       <Identifier>{BE588D62-C6A7-4718-A63D-7B0BDCBD9EEA}</Identifier>
    #         <Identifier>{42D28191-A6E9-4B7B-8C3D-0F0037624F7D}</Identifier>
    #         <Value>Beam3</Value><Identifier>NumeratorBeamChannel</Identifier>
    #         <Value>Beam4</Value><Identifier>DenominatorBeamChannel</Identifier>
    #         <Value>3/2</Value><Identifier>Label</Identifier>
    #         <Value>Raw dH</Value><Identifier>DeltaLabel</Identifier>
    #       <Identifier>ReferencePropertyBag</Identifier>
    #         <Identifier>CalculationPropertyBag</Identifier>
    #         <Value>RatioOffset</Value><Identifier>RatioCalculationMethod</Identifier> 
    #         etc.
    #       <Identifier>SamplePropertyBag</Identifier>
    #         <Value>RatioOffset</Value><Identifier>RatioCalculationMethod</Identifier>
    #         etc.
    #       no direct identifier
    #       <Value>H2</Value><Identifier>Species</Identifier>
    #       <Value>Beam4</Value><Identifier>DetectionBeamChannel</Identifier>
    #       <Value>Peaks</Value><Identifier>EAResultsViewMode</Identifier>
    #       full data grid layout! <Identifier>SerialisedGridLayout</Identifier>
    #       <Value>True</Value><Identifier>ApplyH3CorrectionFactor</Identifier>
    #       <Value>0</Value><Identifier>H3CorrectionFactor</Identifier>
    #         
    #       
    #   <Identifier>E17334B1-4FCD-40E1-9AF0-E5636416ECDB</Identifier> - has information on the isotope standards used for processing (what their isotope values are)
    #   <Identifier>DE9B7164-284D-41A7-94BB-887EA21B760A</Identifier> unclear
    #   <Identifier>BC33520C-B81F-41A5-A18C-26968FEAF7D5</Identifier> unclear
    #   <Identifier>3A451EE6-2446-481A-9E0A-49C913181F2A</Identifier> unclear
    #   
  #   then another <SerialisedPropertyBagProperties> with a bunch of <PersistedPropertyBagProperty>
  #     useful blocks (but nothing with identifiers):
  #       <Value>N2,CO2</Value>
  #       <Identifier>CurrentSpecies</Identifier>
  #       ...
  #       <Value>Acquisition</Value>
  #       <Identifier>GroupMode</Identifier>
  #       ...
  #       <Value>C,N</Value>
  #       <Identifier>CurrentElements</Identifier>
  #       
  #       <Identifier>SerialisedGridLayout</Identifier> has a bunch of table grid layout defining how things look
  #       
  #   
  #   <SerialisedProcessingTask><GlobalIdentifier>0b7e6b49-7756-4cca-b387-4d6617cb28e6</GlobalIdentifier> --> links to specific task with information on peaks and derived quantities (i.e. DATATABLE) - not that useful though for linking beams...
  #   <Value>N2</Value><Identifier>SpeciesName</Identifier>
  #   ApplyH3CorrectionFactor: False/True
  #   H3CorrectionFactor: Value (in ppm/nA?)
  #   PropertyBag Identifier: {42D28191-A6E9-4B7B-8C3D-0F0037624F7D} -- these are the internal unique identifiers for channel definitions
  #     channel definitions, relevant fields NumeratorBeamChannel, DenominatorBeamChannel, Label
  #   OverridenOxygenReferenceRatioValue - potentially useful?
  #   OverridenCarbonReferenceRatioValue - potentially useful?
  #   CarbonCale: VPDB - potentially useful?
  #   Structure:
  #     Identifier: {BE588D62-C6A7-4718-A63D-7B0BDCBD9EEA} -- appear to be ratio definitions from Beam data
  #       underneath: <Identifier>{42D28191-A6E9-4B7B-8C3D-0F0037624F7D}</Identifier> also always the same??
  #       
  # Files: Task_xxxx
  #   ProcessingListTypeIdentifier: this refers to the ProcessingList_1 data
  #   Name: sample name
  #   Id: sample ID (should be unique together with the sample name)
  #   Values:
  #     Metdata --> identifiers for what is what is in methods file (Method_320)
  #   DataSets:
  #     <GlobalIdentifier>0b7e6b49-7756-4cca-b387-4d6617cb28e6</GlobalIdentifier> --> global ID/filename --> shows up in ProcessingList_1 but not with a particularly useful block
  #     Id: --> links to xxx.hdf5 files
  #     AcquireDataStatus: Completed --> check that says "Completed"
  #     AcquireEndDate: exact aquisition end time
  #     AquireStartDate: exact aquisition start time
  #     TypeIdentifier: 
  #         Aquire = IRMS Acquisition
  #         Scan Acquire = IRMS Scan
  #         Readbacks = IRMS Readbacks
  #         Vario EA Results = Elemental Composition
  #         Vario TCD = TCD
  #         7890 FID = FID - 7890 FID
  #     MethodId: 320 --> which method file the data definitions are in
  #     <Identifier>BeamChannel</Identifier> --> some information on UseLowGain yes/no, not in all files!
  #     <Identifier>TuningName</Identifier> and ++ --> information on tuning, not in all files!
  #  File: Method_320: the method definitions file
  #     lots of "FlowParameters" that define sample details:
  #     e.g. Id=4df9279a-1532-46a6-9692-2351391f9db7 --> Name: EAMethod DisplayName: EA Method
  #     e.g. Id=230b3d1b-61c9-4f68-a8cb-917b81776af3 --> Name: EASampleWeight, DisplayName: EA Sample Weight
  #     e.g. Id=961293e6-692e-4f95-a4b2-bed2b3f4c7e1 --> DilutionType
  #     --> probably just parse the "Flows" node completely to get these information out
  #         
}